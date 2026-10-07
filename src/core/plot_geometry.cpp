#include "plot_geometry.h"
#include "string_utils.h"
#include "format_model.h"
#include "histogram_model.h"
#include "model_terms.h"
#include "dataset_model.h"
#include "scatterplot_model.h"
#include "scatter_matrix_model.h"
#include "trellis_scatterplot_model.h"

#include <algorithm>
#include <cmath>
#include <cstdio>
#include <iomanip>
#include <limits>
#include <sstream>

namespace rlispstat {
namespace core {

bool PlotIsDerivedAnalysisView(const PlotModel &model)
{
    return model.isGLMDiagnostic || model.isRegressionDerivedPlot ||
        model.kind == "pca_biplot" || model.kind == "pca_scree" ||
        model.kind == "interaction_plot" || model.kind == "glm_interaction";
}

bool PlotReceivesGlobalAnalysisScopeUpdates(const PlotModel &model)
{
    return !model.isDatasetSeed && !model.analysisScopeFrozen &&
        !PlotIsDerivedAnalysisView(model);
}

void ApplyAnalysisScopeToPlot(PlotModel &model,
                              const AnalysisScope &scope,
                              std::size_t totalRows)
{
    model.dataScope = scope;
    model.dataScopeCaptured = true;
    const auto included = [&](int row) {
        return row <= 0 || AnalysisScopeContainsRow(scope, row, totalRows);
    };

    const std::size_t pointCount = model.points.size();
    std::vector<std::size_t> keptPointIndices;
    keptPointIndices.reserve(pointCount);
    for (std::size_t index = 0; index < pointCount; ++index) {
        if (included(model.points[index].row)) keptPointIndices.push_back(index);
    }
    const auto filterPointAligned = [&](auto &values) {
        if (values.size() != pointCount) return;
        using Value = typename std::decay_t<decltype(values)>::value_type;
        std::vector<Value> filtered;
        filtered.reserve(keptPointIndices.size());
        for (std::size_t index : keptPointIndices) filtered.push_back(values[index]);
        values = std::move(filtered);
    };
    filterPointAligned(model.timeSeriesPointGroups);
    filterPointAligned(model.trellisPointPanels);
    filterPointAligned(model.trellisPointConditionValues);
    filterPointAligned(model.trellisPointCategories);
    filterPointAligned(model.trellisPointSplitValues);

    model.points.erase(std::remove_if(model.points.begin(), model.points.end(),
        [&](const DataPoint &point) { return !included(point.row); }), model.points.end());
    if (model.kind == "scatter" && !PlotIsDerivedAnalysisView(model))
        ComputeRanges(model);
    if (model.kind == "time_series" && !PlotIsDerivedAnalysisView(model)) {
        model.interactionPlotLines = BuildTimeSeriesLines(
            model.points, model.timeSeriesPointGroups, model.yLabel);
        ComputeRanges(model);
    }
    model.boxplotPoints.erase(std::remove_if(model.boxplotPoints.begin(), model.boxplotPoints.end(),
        [&](const BoxplotPoint &point) { return !included(point.row); }), model.boxplotPoints.end());
    const std::size_t originalHistogramBinCount = model.histogramBins.size();
    std::vector<double> originalHistogramValues;
    originalHistogramValues.reserve(model.histogramPoints.size());
    for (const HistogramPoint &point : model.histogramPoints) {
        if (std::isfinite(point.x)) originalHistogramValues.push_back(point.x);
    }
    const bool histogramUsedAutomaticBins = originalHistogramBinCount > 0 &&
        originalHistogramBinCount == static_cast<std::size_t>(
            HistogramBinCountForRule(originalHistogramValues, "sturges"));
    model.histogramPoints.erase(std::remove_if(model.histogramPoints.begin(), model.histogramPoints.end(),
        [&](const HistogramPoint &point) { return !included(point.row); }), model.histogramPoints.end());
    if (originalHistogramBinCount > 0) {
        if (model.histogramPoints.empty()) {
            model.histogramBins.clear();
        } else if (histogramUsedAutomaticBins) {
            // Automatic binning is part of the histogram calculation, so its
            // count as well as its domain must be derived from the scoped data.
            RebinHistogramByRule(model, "sturges");
        } else {
            // An explicitly requested bin count remains a user option, but
            // the breaks are rebuilt from the scoped minimum and maximum.
            RebinHistogram(model, static_cast<int>(originalHistogramBinCount));
        }
    }

    for (BarplotBin &bin : model.barplotBins) {
        bin.rows.erase(std::remove_if(bin.rows.begin(), bin.rows.end(),
            [&](int row) { return !included(row); }), bin.rows.end());
        bin.n = static_cast<int>(bin.rows.size());
        for (BarplotSegment &segment : bin.segments) {
            segment.rows.erase(std::remove_if(segment.rows.begin(), segment.rows.end(),
                [&](int row) { return !included(row); }), segment.rows.end());
            segment.count = static_cast<int>(segment.rows.size());
            segment.barN = bin.n;
        }
    }
    model.barplotBins.erase(
        std::remove_if(model.barplotBins.begin(), model.barplotBins.end(),
            [](const BarplotBin &bin) { return bin.n == 0; }),
        model.barplotBins.end());
    if (!model.barplotSplitVariable.empty()) {
        std::vector<std::string> observedLevels;
        for (const BarplotBin &bin : model.barplotBins) {
            for (const BarplotSegment &segment : bin.segments) {
                if (segment.count > 0 &&
                    std::find(observedLevels.begin(), observedLevels.end(), segment.level) ==
                        observedLevels.end()) {
                    observedLevels.push_back(segment.level);
                }
            }
        }
        for (BarplotBin &bin : model.barplotBins) {
            bin.segments.erase(
                std::remove_if(bin.segments.begin(), bin.segments.end(),
                    [&](const BarplotSegment &segment) {
                        return std::find(observedLevels.begin(), observedLevels.end(),
                                         segment.level) == observedLevels.end();
                    }),
                bin.segments.end());
        }
    }
    int scopedTotal = 0;
    for (const BarplotBin &bin : model.barplotBins) scopedTotal += bin.n;
    model.barplotTotalN = scopedTotal;
    for (BarplotBin &bin : model.barplotBins) {
        bin.percent = scopedTotal > 0 ? 100.0 * bin.n / scopedTotal : 0.0;
        bin.widthValue = model.barplotWidthMode == "proportional_percent"
            ? bin.percent : static_cast<double>(bin.n);
        for (BarplotSegment &segment : bin.segments) {
            segment.totalN = scopedTotal;
            segment.conditionalPercent = bin.n > 0 ? 100.0 * segment.count / bin.n : 0.0;
            segment.overallPercent = scopedTotal > 0 ? 100.0 * segment.count / scopedTotal : 0.0;
            segment.barPercent = bin.percent;
            segment.barWidthValue = bin.widthValue;
        }
    }
}

bool IsValidRect(const Rect &rect)
{
    return std::isfinite(rect.x) && std::isfinite(rect.y) &&
        std::isfinite(rect.width) && std::isfinite(rect.height) &&
        rect.width > 0.0 && rect.height > 0.0;
}

Rect ZeroBaselineContentRect(const Rect &plotRect)
{
    if (!IsValidRect(plotRect)) return plotRect;
    constexpr double sideFraction = 0.025;
    constexpr double topFraction = 0.04;
    constexpr double bottomFraction = 0.025;
    return {plotRect.x + plotRect.width * sideFraction,
            plotRect.y + plotRect.height * topFraction,
            plotRect.width * (1.0 - 2.0 * sideFraction),
            plotRect.height * (1.0 - topFraction - bottomFraction)};
}

double ZeroBaselineY(const Rect &plotRect, double fractionOfMaximum)
{
    const Rect content = ZeroBaselineContentRect(plotRect);
    return content.y + content.height * (1.0 - fractionOfMaximum);
}

bool IsValidViewport(const DataViewport &viewport)
{
    return std::isfinite(viewport.xmin) && std::isfinite(viewport.xmax) &&
        std::isfinite(viewport.ymin) && std::isfinite(viewport.ymax) &&
        viewport.xmax != viewport.xmin && viewport.ymax != viewport.ymin;
}

DataViewport DataViewportForPoints(const std::vector<Point> &points,
                                   double paddingFraction)
{
    bool found = false;
    DataViewport viewport;
    for (const Point &point : points) {
        if (!std::isfinite(point.x) || !std::isfinite(point.y)) {
            continue;
        }
        if (!found) {
            viewport.xmin = viewport.xmax = point.x;
            viewport.ymin = viewport.ymax = point.y;
            found = true;
        } else {
            viewport.xmin = std::min(viewport.xmin, point.x);
            viewport.xmax = std::max(viewport.xmax, point.x);
            viewport.ymin = std::min(viewport.ymin, point.y);
            viewport.ymax = std::max(viewport.ymax, point.y);
        }
    }
    if (!found) {
        return {};
    }
    if (viewport.xmin == viewport.xmax) {
        viewport.xmin -= 0.5;
        viewport.xmax += 0.5;
    }
    if (viewport.ymin == viewport.ymax) {
        viewport.ymin -= 0.5;
        viewport.ymax += 0.5;
    }
    const double pad = std::max(0.0, paddingFraction);
    const double xpad = (viewport.xmax - viewport.xmin) * pad;
    const double ypad = (viewport.ymax - viewport.ymin) * pad;
    viewport.xmin -= xpad;
    viewport.xmax += xpad;
    viewport.ymin -= ypad;
    viewport.ymax += ypad;
    return viewport;
}

Point DataToScreen(const Point &data,
                   const DataViewport &viewport,
                   const Rect &plotRect,
                   bool yAxisDown)
{
    if (!IsValidViewport(viewport) || !IsValidRect(plotRect)) {
        return {NAN, NAN};
    }
    const double xFraction = (data.x - viewport.xmin) / (viewport.xmax - viewport.xmin);
    const double yFraction = (data.y - viewport.ymin) / (viewport.ymax - viewport.ymin);
    const double screenX = plotRect.x + xFraction * plotRect.width;
    const double screenY = yAxisDown
        ? plotRect.y + plotRect.height - yFraction * plotRect.height
        : plotRect.y + yFraction * plotRect.height;
    return {screenX, screenY};
}

Point ScreenToData(const Point &screen,
                   const DataViewport &viewport,
                   const Rect &plotRect,
                   bool yAxisDown)
{
    if (!IsValidViewport(viewport) || !IsValidRect(plotRect)) {
        return {NAN, NAN};
    }
    const double xFraction = (screen.x - plotRect.x) / plotRect.width;
    const double yFraction = yAxisDown
        ? (plotRect.y + plotRect.height - screen.y) / plotRect.height
        : (screen.y - plotRect.y) / plotRect.height;
    return {
        viewport.xmin + xFraction * (viewport.xmax - viewport.xmin),
        viewport.ymin + yFraction * (viewport.ymax - viewport.ymin)
    };
}

bool PointInRect(const Point &point, const Rect &rect)
{
    return point.x >= rect.x && point.x <= rect.x + rect.width &&
        point.y >= rect.y && point.y <= rect.y + rect.height;
}

bool RectIntersects(const Rect &a, const Rect &b)
{
    if (!IsValidRect(a) || !IsValidRect(b)) {
        return false;
    }
    return a.x <= b.x + b.width && a.x + a.width >= b.x &&
        a.y <= b.y + b.height && a.y + a.height >= b.y;
}

bool ClipLineToRect(Point &start, Point &end, const Rect &clip)
{
    if (!IsValidRect(clip) || !std::isfinite(start.x) || !std::isfinite(start.y) ||
        !std::isfinite(end.x) || !std::isfinite(end.y)) return false;
    const double dx = end.x - start.x;
    const double dy = end.y - start.y;
    double lower = 0.0;
    double upper = 1.0;
    auto constrain = [&](double p, double q) {
        if (p == 0.0) return q >= 0.0;
        const double ratio = q / p;
        if (p < 0.0) {
            if (ratio > upper) return false;
            lower = std::max(lower, ratio);
        } else {
            if (ratio < lower) return false;
            upper = std::min(upper, ratio);
        }
        return true;
    };
    if (!constrain(-dx, start.x - clip.x) ||
        !constrain(dx, clip.x + clip.width - start.x) ||
        !constrain(-dy, start.y - clip.y) ||
        !constrain(dy, clip.y + clip.height - start.y) || lower > upper) return false;
    const Point original = start;
    start = {original.x + lower * dx, original.y + lower * dy};
    end = {original.x + upper * dx, original.y + upper * dy};
    const double right = clip.x + clip.width;
    const double bottom = clip.y + clip.height;
    start.x = std::max(clip.x, std::min(right, start.x));
    start.y = std::max(clip.y, std::min(bottom, start.y));
    end.x = std::max(clip.x, std::min(right, end.x));
    end.y = std::max(clip.y, std::min(bottom, end.y));
    return true;
}

Rect ExpandRect(const Rect &rect, double amount)
{
    return ExpandRect(rect, amount, amount);
}

Rect ExpandRect(const Rect &rect, double xAmount, double yAmount)
{
    return {
        rect.x - xAmount,
        rect.y - yAmount,
        rect.width + xAmount * 2.0,
        rect.height + yAmount * 2.0
    };
}

Rect RectBetweenPoints(const Point &a, const Point &b)
{
    return {
        std::min(a.x, b.x),
        std::min(a.y, b.y),
        std::fabs(a.x - b.x),
        std::fabs(a.y - b.y)
    };
}

Rect BrushRectForGesture(const Point &start,
                         const Point &current,
                         bool useFixedBrush,
                         double brushWidth,
                         double brushHeight,
                         double clickThreshold)
{
    (void)clickThreshold;
    Rect rect = RectBetweenPoints(start, current);
    if (useFixedBrush) {
        const double width = std::max(0.0, brushWidth);
        const double height = std::max(0.0, brushHeight);
        return {
            current.x - width / 2.0,
            current.y - height / 2.0,
            width,
            height
        };
    }
    return rect;
}

bool PlotInteractionModeAllowsRowSelection(const std::string &mode)
{
    return mode == "select" || mode == "brush";
}

bool RectMeetsMinimumSize(const Rect &rect, double minimumWidth, double minimumHeight)
{
    return rect.width >= minimumWidth && rect.height >= minimumHeight;
}

double DistanceSquared(const Point &a, const Point &b)
{
    const double dx = a.x - b.x;
    const double dy = a.y - b.y;
    return dx * dx + dy * dy;
}

double DistanceSquaredToRect(const Point &point, const Rect &rect)
{
    if (!IsValidRect(rect)) {
        return NAN;
    }
    double dx = 0.0;
    double dy = 0.0;
    if (point.x < rect.x) {
        dx = rect.x - point.x;
    } else if (point.x > rect.x + rect.width) {
        dx = point.x - (rect.x + rect.width);
    }
    if (point.y < rect.y) {
        dy = rect.y - point.y;
    } else if (point.y > rect.y + rect.height) {
        dy = point.y - (rect.y + rect.height);
    }
    return dx * dx + dy * dy;
}

double DistanceSquaredToSegment(const Point &point,
                                const Point &start,
                                const Point &end)
{
    if (!std::isfinite(point.x) || !std::isfinite(point.y) ||
        !std::isfinite(start.x) || !std::isfinite(start.y) ||
        !std::isfinite(end.x) || !std::isfinite(end.y)) {
        return NAN;
    }
    const double vx = end.x - start.x;
    const double vy = end.y - start.y;
    const double wx = point.x - start.x;
    const double wy = point.y - start.y;
    const double denom = vx * vx + vy * vy;
    const double t = denom > 0.0
        ? std::max(0.0, std::min(1.0, (wx * vx + wy * vy) / denom))
        : 0.0;
    const Point projection{start.x + t * vx, start.y + t * vy};
    return DistanceSquared(point, projection);
}

int HitNearestIndexedPoint(const Point &point,
                           const std::vector<IndexedScreenPoint> &candidates,
                           double maxDistance)
{
    int hitIndex = 0;
    double bestDistanceSquared = maxDistance * maxDistance;
    for (const IndexedScreenPoint &candidate : candidates) {
        if (candidate.index <= 0) {
            continue;
        }
        const double distanceSquared = DistanceSquared(point, candidate.point);
        if (std::isfinite(distanceSquared) && distanceSquared <= bestDistanceSquared) {
            bestDistanceSquared = distanceSquared;
            hitIndex = candidate.index;
        }
    }
    return hitIndex;
}

std::string HitNearestLabeledSegment(const Point &point,
                                     const std::vector<LabeledScreenSegment> &candidates,
                                     double maxDistance)
{
    std::string hitId;
    double bestDistanceSquared = maxDistance * maxDistance;
    for (const LabeledScreenSegment &candidate : candidates) {
        if (candidate.id.empty()) {
            continue;
        }
        double distanceSquared = DistanceSquaredToSegment(point, candidate.start, candidate.end);
        const double endDistanceSquared = DistanceSquared(point, candidate.end);
        if (std::isfinite(endDistanceSquared)) {
            distanceSquared = std::isfinite(distanceSquared)
                ? std::min(distanceSquared, endDistanceSquared)
                : endDistanceSquared;
        }
        if (candidate.hasLabelRect && PointInRect(point, candidate.labelRect)) {
            distanceSquared = 0.0;
        }
        if (std::isfinite(distanceSquared) && distanceSquared <= bestDistanceSquared) {
            bestDistanceSquared = distanceSquared;
            hitId = candidate.id;
        }
    }
    return hitId;
}

NumericVariable *FindNumericVariable(std::vector<NumericVariable> &vars, const std::string &name)
{
    for (NumericVariable &v : vars) {
        if (v.name == name) return &v;
    }
    return nullptr;
}

VariableMeta *FindVariableMeta(std::vector<VariableMeta> &metas, const std::string &name)
{
    for (VariableMeta &m : metas) {
        if (m.name == name) return &m;
    }
    return nullptr;
}

std::vector<std::string> VariableNames(const std::vector<VariableMeta> &metas)
{
    std::vector<std::string> names;
    names.reserve(metas.size());
    for (const VariableMeta &m : metas) names.push_back(m.name);
    return names;
}

std::vector<std::string> NumericVariableNames(const std::vector<NumericVariable> &vars)
{
    std::vector<std::string> names;
    names.reserve(vars.size());
    for (const NumericVariable &v : vars) names.push_back(v.name);
    return names;
}

bool VariableExists(const std::vector<VariableMeta> &metas, const std::string &name)
{
    for (const VariableMeta &m : metas) {
        if (m.name == name) return true;
    }
    return false;
}

NumericVariable *FindNumericVariable(PlotModel &model, const std::string &name)
{
    for (NumericVariable &var : model.variables) {
        if (var.name == name) return &var;
    }
    return nullptr;
}

const NumericVariable *FindNumericVariable(const PlotModel &model, const std::string &name)
{
    for (const NumericVariable &var : model.variables) {
        if (var.name == name) return &var;
    }
    return nullptr;
}

const VariableMeta *FindVariableMeta(const PlotModel &model, const std::string &name)
{
    for (const VariableMeta &var : model.variableMeta) {
        if (var.name == name) return &var;
    }
    return nullptr;
}

std::vector<std::string> VariableNames(const PlotModel &model)
{
    return VariableNames(model.variableMeta);
}

std::vector<std::string> NumericVariableNames(const PlotModel &model)
{
    return NumericVariableNames(model.variables);
}

bool VariableExists(const PlotModel &model, const std::string &name)
{
    for (const VariableMeta &var : model.variableMeta) {
        if (var.name == name) return true;
    }
    for (const NumericVariable &var : model.variables) {
        if (var.name == name) return true;
    }
    return false;
}

std::vector<std::string> AvailableVariableNames(const PlotModel &model,
                                                const DataFrameModel *df)
{
    std::vector<std::string> names = VariableNames(model);
    if (df) {
        for (const DataColumn &col : df->columns) {
            AppendUniqueString(names, col.name);
        }
    }
    return names;
}

bool AvailableVariableExists(const PlotModel &model,
                             const std::string &name,
                             const DataFrameModel *df)
{
    if (name.empty()) {
        return false;
    }
    if (VariableExists(model, name)) {
        return true;
    }
    return df && FindDataColumnInDataFrame(*df, name);
}

std::string VariableRole(const PlotModel &model, const std::string &variableName)
{
    return VariableRoleForModelContext(variableName, "", {}, model.xLabel, model.yLabel);
}

std::string VariableRole(const PlotModel &model,
                         const std::string &variableName,
                         const std::string &dependent,
                         const std::vector<std::string> &terms)
{
    return VariableRoleForModelContext(variableName, dependent, terms, model.xLabel, model.yLabel);
}

std::string DefaultTermType(const PlotModel &model, const std::string &term)
{
    if (IsInteractionTerm(term)) {
        return "interaction";
    }
    if (IsPolynomialTerm(term)) {
        return "numeric";
    }
    std::string variable = BaseVariableForTermComponent(term);
    if (variable != TrimCopy(term)) {
        return "factor";
    }
    const VariableMeta *meta = FindVariableMeta(model, variable);
    if (meta && VariableTypeIsFactorLike(meta->type)) {
        return "factor";
    }
    return "numeric";
}

std::string ModelTermDisplayType(const PlotModel &model,
                                 const std::string &term,
                                 const std::map<std::string, std::string> &overrides)
{
    if (IsInteractionTerm(term)) {
        std::vector<std::string> componentTypes;
        for (const std::string &part : SplitInteractionTerm(term)) {
            std::string component = BaseVariableForTermComponent(part);
            componentTypes.push_back(ResolvedModelTermDisplayType(
                component, overrides, DefaultTermType(model, component)));
        }
        return SemanticInteractionTermType(componentTypes);
    }
    return ResolvedModelTermDisplayType(term, overrides, DefaultTermType(model, term));
}

std::string TermDisplayType(const std::string &term, const std::string &type, const std::string &)
{
    return ResolvedModelTermDisplayType(term, {}, type);
}

bool PlotIsAggregateDiagnostic(const PlotModel &model)
{
    return model.glmDiagnosticKind == "boundary_zero_fit" ||
           model.glmDiagnosticKind == "observed_predicted_score_distribution";
}

bool PlotHasConfidenceIntervals(const PlotModel &model)
{
    return std::any_of(model.interactionPlotLines.begin(),
                       model.interactionPlotLines.end(), [](const auto &series) {
        return !series.points.empty() &&
               series.confidenceLower.size() == series.points.size() &&
               series.confidenceUpper.size() == series.points.size();
    });
}

bool PlotIsImputationDiagnostic(const PlotModel &model)
{
    return model.glmDiagnosticKind == "scale_score_distribution" ||
        model.glmDiagnosticKind == "imputation_distributions" ||
        model.glmDiagnosticKind == "imputation_chain_mean" ||
        model.glmDiagnosticKind == "imputation_chain_variance";
}

void SelectDiagnosticPlotSeries(PlotModel &model, std::optional<std::size_t> series, bool extend)
{
    if (!PlotIsImputationDiagnostic(model)) return;
    if (!extend) model.selectedDiagnosticSeries.clear();
    if (!series || *series >= model.interactionPlotLines.size()) return;
    if (extend && model.selectedDiagnosticSeries.count(*series))
        model.selectedDiagnosticSeries.erase(*series);
    else model.selectedDiagnosticSeries.insert(*series);
}

std::set<int> DiagnosticPlotSelectedPointIds(const PlotModel &model)
{
    std::set<int> result;
    if (!PlotIsImputationDiagnostic(model)) return result;
    for (auto index : model.selectedDiagnosticSeries) {
        if (index >= model.interactionPlotLines.size()) continue;
        for (const auto &point : model.interactionPlotLines[index].points)
            if (point.row > 0) result.insert(point.row);
    }
    return result;
}

std::vector<std::pair<int, std::string>> ImputationDiagnosticBatchOptions(const PlotModel &model)
{
    std::vector<std::pair<int, std::string>> options;
    const int total=model.imputationDiagnosticTotal;
    if (total<=0) return options;
    std::set<int> starts;
    if(total<=100) for(int i=1;i<=total;i+=5) starts.insert(i);
    else {
        starts={1,model.imputationDiagnosticFirst,1+5*((total-1)/5)};
        if(model.imputationDiagnosticFirst>5) starts.insert(model.imputationDiagnosticFirst-5);
        if(model.imputationDiagnosticLast<total) starts.insert(model.imputationDiagnosticLast+1);
    }
    for(int first:starts) options.push_back({first,"Imputations "+std::to_string(first)+
        "–"+std::to_string(std::min(total,first+4))});
    return options;
}

std::string ImputationDiagnosticDisplayStatus(const PlotModel &model)
{
    if (model.glmDiagnosticKind == "scale_score_distribution")
        return "Histogram proportions; each line describes one completed dataset, not individual cases.";
    if (model.glmDiagnosticKind == "scale_dimensionality")
        return model.diagnosticImputationCount > 1 ? "Showing imputation " +
            std::to_string(model.diagnosticImputationIndex) + " of " +
            std::to_string(model.diagnosticImputationCount) + " (not pooled)." : "";
    if(model.imputationDiagnosticTotal<=0) return "";
    return "Observed + imputations "+std::to_string(model.imputationDiagnosticFirst)+"–"+
        std::to_string(model.imputationDiagnosticLast)+" of "+std::to_string(model.imputationDiagnosticTotal)+
        (model.imputationDiagnosticSimplified ? " · Simplified ECDF display (all values used)" : "");
}

std::string TimeSeriesPlotExplanation(const PlotModel &model)
{
    if (model.glmDiagnosticKind == "imputation_distributions")
        return "Observed versus imputed values\n\n"
            "X shows the value of the variable. Y shows the cumulative proportion at or below that value: 0.8 means 80%. Each curve reaches 1 (100%).\n\n"
            "Observed uses the originally observed values. Each Imputed curve uses only the values filled in for originally missing cells in that imputation. These are separate distributions, not a pooled estimate.\n\n"
            "At most five imputations are displayed at once. Use Imputations shown to change the set. For large samples, the display uses at most 512 ECDF steps per curve at R type-1 quantiles. Heights are evaluated with stats::ecdf on every finite value; steps between retained points are a display approximation. No cases are sampled. R Code draws the exact ECDF for the same displayed imputations.\n\n"
            "Steps are expected, especially with few missing cases or repeated values. Higher curves at the same X indicate more values at or below that threshold. Curves shifted to the right indicate larger values.\n\n"
            "Look for implausible values, unexpected shifts or very different spreads, and compare the imputations. Observed and imputed distributions need not match: missing cases may differ systematically. Similar curves do not prove that the imputations are valid or that the algorithm converged. Inspect chain plots and logged events too.\n\n"
            "Click a curve or its legend entry to highlight it. Shift-click toggles additional curves; click empty plot space to clear the highlight. This does not select source data rows. Choose Legend under Series identification, then drag the legend to reposition it.\n\n"
            "Reference: https://amices.org/mice/reference/densityplot.mids.html (observed/imputed comparisons; this plot uses an ECDF rather than density smoothing).";
    if (PlotIsImputationDiagnostic(model))
        return std::string("Imputation chain trace\n\nX shows the iteration. Y shows the ") +
            (model.glmDiagnosticKind == "imputation_chain_variance" ? "variance" : "mean") +
            " of the imputed values. Each coloured line is one imputation chain; it is not a subject's time series.\n\n"
            "Look for persistent trends, sudden changes or chains that remain separated. Stable, overlapping traces are reassuring but do not prove convergence or valid imputations. Short runs provide limited evidence; inspect other diagnostics and the imputation model. These summaries are not Rubin-pooled.\n\n"
            "Click a curve or its legend entry to highlight it; Shift-click toggles additional curves. Click empty plot space to clear the highlight. These actions do not select source data rows. Choose Legend under Series identification and drag it to reposition it.\n\nReference: https://amices.org/mice/reference/plot.mids.html";
    return "Time series\n\nX is the time or ordering variable; Y is the measured value. Lines connect observations in X order within each series. The legend identifies those series.\n\n"
        "Look for trends, changes, cycles and unusual observations. A connecting line is a visual guide: it is not a fitted model or evidence of causation. Unequal time gaps matter, and missing or unobserved intervals are not filled in by this plot.\n\n"
        "Choose Legend under Series identification and drag it to reposition it. Clicking a legend entry selects the data rows belonging to that series when the plot is linked to data.";
}

bool SetTimeSeriesLegendPosition(PlotModel &model, const std::string &position)
{
    if (position != "top_left" && position != "top_right" &&
        position != "bottom_left" && position != "bottom_right" &&
        position != "left" && position != "right" && position != "top" && position != "bottom") return false;
    model.timeSeriesLegendPosition = position;
    model.timeSeriesIdentification = "legend";
    model.interactionLegendUsesCustomPosition = false;
    return true;
}

bool PlotLinksToDataRows(const PlotModel &model)
{
    return model.kind != "pca_scree" && !PlotIsAggregateDiagnostic(model) && !PlotIsImputationDiagnostic(model);
}

bool PlotSupportsCaseExclusion(const PlotModel &model)
{
    if (model.isDatasetSeed || !PlotLinksToDataRows(model)) return false;
    return !PlotIsDerivedAnalysisView(model) || model.isGLMDiagnostic;
}

bool PlotSupportsObservationLabels(const PlotModel &model)
{
    if (PlotIsAggregateDiagnostic(model)) return false;
    if (model.kind == "trellis_scatterplot")
        return !model.trellisSpecificationInitialized ||
               model.trellisSpecification.plotType == TrellisPlotType::Scatter;
    return model.kind == "scatter" || model.kind == "scatter_matrix" ||
           model.kind == "histogram";
}

std::string TitleForPlot(const PlotModel &model)
{
    if (model.kind == "trellis_scatterplot") {
        return DeriveTrellisMetadata(model).windowTitle;
    }
    return PlotWindowTitle(model.title, model.yLabel, model.xLabel,
                           model.group, model.interactionMode, model.selectionMode);
}

std::string PlotListItemText(const PlotModel &model)
{
    std::ostringstream out;
    out << model.id << "|" << model.group << "|" << model.xLabel << "|"
        << model.yLabel << "|" << model.points.size() << "|" << model.selectionMode
        << "|" << model.interactionMode;
    return out.str();
}

std::string PlotInfoResponseText(const PlotModel &model, std::size_t selectedCount)
{
    std::ostringstream out;
    out << model.id << "|" << model.group << "|" << model.xLabel << "|"
        << model.yLabel << "|" << model.points.size() << "|" << selectedCount << "|"
        << model.xmin << "|" << model.xmax << "|" << model.ymin << "|" << model.ymax
        << "|" << model.selectionMode << "|" << model.interactionMode << "|"
        << model.title;
    return out.str();
}

std::string PlotVariablesResponseText(const PlotModel &model)
{
    std::ostringstream out;
    out << "OK";
    for (const NumericVariable &var : model.variables) {
        out << "\t" << var.name;
    }
    return out.str();
}

std::string PlotOverlaysResponseText(const PlotModel &model)
{
    std::ostringstream out;
    out << "OK";
    for (const Overlay &overlay : model.overlays) {
        out << "\t" << overlay.id << "|" << overlay.type << "|" << overlay.source << "|"
            << (overlay.visible ? "TRUE" : "FALSE");
    }
    return out.str();
}

std::string PlotDiagnosticInfoResponseText(const PlotModel &model)
{
    const std::size_t diagnosticCount =
        model.kind == "histogram" ? model.histogramPoints.size() : model.points.size();
    std::ostringstream out;
    out << "OK\tDIAGNOSTIC|" << model.id << "|" << model.group << "|" << model.glmModelId
        << "|" << model.glmDiagnosticKind << "|" << model.displayedFitVersion
        << "|" << model.displayedDiagnosticsVersion << "|" << diagnosticCount;
    const std::size_t preview = std::min<std::size_t>(3, diagnosticCount);
    for (std::size_t i = 0; i < preview; ++i) {
        if (model.kind == "histogram") {
            out << "|" << model.histogramPoints[i].row << "," << FormatDouble(model.histogramPoints[i].x, 6)
                << ",bin" << model.histogramPoints[i].bin;
        } else {
            out << "|" << model.points[i].row << "," << FormatDouble(model.points[i].x, 6)
                << "," << FormatDouble(model.points[i].y, 6);
        }
    }
    return out.str();
}

void SetVariableMetaType(std::vector<VariableMeta> &meta, const std::string &variable, const std::string &type)
{
    for (VariableMeta &entry : meta) {
        if (entry.name == variable) {
            entry.type = type;
            return;
        }
    }
    meta.push_back(VariableMeta{variable, type});
}

bool HasOverlaySource(const PlotModel &model, const std::string &source)
{
    for (const Overlay &overlay : model.overlays) {
        if (overlay.type == "lm" && overlay.source == source && overlay.visible) {
            return true;
        }
    }
    return false;
}

void AddOverlaySource(PlotModel &model, const std::string &source)
{
    if (HasOverlaySource(model, source)) {
        return;
    }
    model.overlays.push_back(Overlay{model.nextOverlayId++, "lm", source, true});
    if (model.kind == "scatter_matrix") {
        InvalidateScatterMatrixFits(model);
        return;
    }
    SmoothCurveScope scope = SmoothCurveScope::Overall;
    if (source == "selected") scope = SmoothCurveScope::Selection;
    else if (source == "color") scope = SmoothCurveScope::ColorGroup;
    if (!FitCurveScopeIsPresent(model.smoothCurves, scope, "lm"))
        model.smoothCurves.push_back(PendingSmoothCurve(scope, "lm"));
}

void RemoveOverlaySource(PlotModel &model, const std::string &source)
{
    model.overlays.erase(
        std::remove_if(model.overlays.begin(), model.overlays.end(), [&](const Overlay &overlay) {
            return overlay.type == "lm" && overlay.source == source;
        }),
        model.overlays.end()
    );
    if (model.kind == "scatter_matrix") {
        InvalidateScatterMatrixFits(model);
        return;
    }
    SmoothCurveScope scope = SmoothCurveScope::Overall;
    if (source == "selected") scope = SmoothCurveScope::Selection;
    else if (source == "color") scope = SmoothCurveScope::ColorGroup;
    model.smoothCurves.erase(
        std::remove_if(model.smoothCurves.begin(), model.smoothCurves.end(),
            [&](const SmoothCurveData &curve) {
                return curve.scope == scope && curve.fitMethod == "lm";
            }), model.smoothCurves.end());
}

SmoothCurveData PendingSmoothCurve(SmoothCurveScope scope,
                                   const std::string &fitMethod)
{
    SmoothCurveData pending;
    pending.scope = scope;
    pending.fitMethod = fitMethod;
    pending.groupId = ".";
    pending.ok = false;
    pending.message = "needed";
    return pending;
}

SmoothCurveData EnabledEmptySmoothCurve(SmoothCurveScope scope,
                                        const std::string &fitMethod)
{
    SmoothCurveData enabled;
    enabled.scope = scope;
    enabled.fitMethod = fitMethod;
    enabled.groupId = ".";
    enabled.ok = false;
    // An empty message distinguishes an enabled scope with no eligible cases
    // from a request that still needs to be sent to R.
    enabled.message.clear();
    return enabled;
}

double ClampSmoothSpan(double span)
{
    if (!std::isfinite(span)) {
        return 0.75;
    }
    return std::max(0.20, std::min(2.00, span));
}

bool SmoothCurveScopeIsPresent(const std::vector<SmoothCurveData> &curves,
                               SmoothCurveScope scope)
{
    return FitCurveScopeIsPresent(curves, scope, "loess");
}

bool FitCurveScopeIsPresent(const std::vector<SmoothCurveData> &curves,
                            SmoothCurveScope scope,
                            const std::string &fitMethod,
                            bool requireValid)
{
    return std::any_of(curves.begin(), curves.end(),
        [&](const SmoothCurveData &curve) {
            return curve.scope == scope && curve.fitMethod == fitMethod &&
                (!requireValid || curve.ok);
        });
}

bool MarkSmoothCurveScopePendingIfPresent(std::vector<SmoothCurveData> &curves,
                                          SmoothCurveScope scope)
{
    std::vector<std::string> methods;
    for (const SmoothCurveData &curve : curves) {
        if (curve.scope == scope &&
            std::find(methods.begin(), methods.end(), curve.fitMethod) == methods.end())
            methods.push_back(curve.fitMethod);
    }
    if (methods.empty()) {
        return false;
    }
    // Keep the last valid curve visible while R recomputes it. Only an older
    // pending marker is replaced; the R reply atomically replaces the scope.
    curves.erase(std::remove_if(curves.begin(), curves.end(),
                                [&](const SmoothCurveData &curve) {
                                    return curve.scope == scope && !curve.ok;
                                }),
                 curves.end());
    for (const std::string &method : methods)
        curves.push_back(PendingSmoothCurve(scope, method));
    return true;
}

bool MarkExistingSmoothCurvesPending(std::vector<SmoothCurveData> &curves)
{
    std::vector<std::pair<SmoothCurveScope, std::string>> enabled;
    for (const SmoothCurveData &curve : curves) {
        const auto key = std::make_pair(curve.scope, curve.fitMethod);
        if (std::find(enabled.begin(), enabled.end(), key) == enabled.end())
            enabled.push_back(key);
    }
    if (enabled.empty()) {
        return false;
    }
    curves.erase(std::remove_if(curves.begin(), curves.end(),
                                [](const SmoothCurveData &curve) { return !curve.ok; }),
                 curves.end());
    for (const auto &entry : enabled)
        curves.push_back(PendingSmoothCurve(entry.first, entry.second));
    return true;
}

double ClampPointSizeScale(double scale)
{
    if (!std::isfinite(scale)) return 1.0;
    return std::max(0.50, std::min(3.00, scale));
}

bool InvalidateSmoothCurvesForCoordinateChange(
    std::vector<SmoothCurveData> &curves)
{
    std::vector<std::pair<SmoothCurveScope, std::string>> enabled;
    for (const SmoothCurveData &curve : curves) {
        const auto key = std::make_pair(curve.scope, curve.fitMethod);
        if (std::find(enabled.begin(), enabled.end(), key) == enabled.end())
            enabled.push_back(key);
    }
    if (enabled.empty()) {
        return false;
    }

    curves.clear();
    for (const auto &entry : enabled)
        curves.push_back(PendingSmoothCurve(entry.first, entry.second));
    return true;
}

bool ToggleSmoothCurveScopePending(std::vector<SmoothCurveData> &curves,
                                   SmoothCurveScope scope)
{
    const std::size_t oldSize = curves.size();
    curves.erase(std::remove_if(curves.begin(), curves.end(),
                                [&](const SmoothCurveData &curve) {
                                    return curve.scope == scope && curve.fitMethod == "loess";
                                }),
                 curves.end());
    if (curves.size() != oldSize) {
        return true;
    }
    curves.push_back(PendingSmoothCurve(scope));
    return true;
}

std::vector<InteractionPlotLine> BuildTimeSeriesLines(
    const std::vector<DataPoint> &points,
    const std::vector<std::string> &pointGroups,
    const std::string &defaultLabel)
{
    std::vector<InteractionPlotLine> lines;
    std::map<std::string, std::size_t> indexByLabel;
    const bool grouped = pointGroups.size() == points.size();
    for (std::size_t index = 0; index < points.size(); ++index) {
        std::string label = grouped ? pointGroups[index] : defaultLabel;
        if (label.empty()) label = defaultLabel.empty() ? "Series" : defaultLabel;
        auto found = indexByLabel.find(label);
        if (found == indexByLabel.end()) {
            const std::size_t lineIndex = lines.size();
            indexByLabel[label] = lineIndex;
            InteractionPlotLine line;
            line.label = label;
            line.colorKey = grouped ? PaletteColorNameAtIndex(lineIndex) : "black";
            lines.push_back(std::move(line));
            found = indexByLabel.find(label);
        }
        lines[found->second].points.push_back(points[index]);
        if (points[index].row > 0) {
            lines[found->second].caseIds.push_back(points[index].row);
        }
    }
    for (InteractionPlotLine &line : lines) {
        std::stable_sort(line.points.begin(), line.points.end(),
                         [](const DataPoint &left, const DataPoint &right) {
                             if (left.x != right.x) return left.x < right.x;
                             return left.row < right.row;
                         });
        std::sort(line.caseIds.begin(), line.caseIds.end());
        line.caseIds.erase(std::unique(line.caseIds.begin(), line.caseIds.end()),
                           line.caseIds.end());
    }
    return lines;
}

std::set<int> PlotColorLegendRows(const PlotModel &plot,
                                  const std::string &level)
{
    const auto semantic = plot.colorByLegendRows.find(level);
    if (semantic != plot.colorByLegendRows.end()) {
        return std::set<int>(semantic->second.begin(), semantic->second.end());
    }

    // Backward-compatible fallback for plot snapshots created before the
    // semantic map existed. New live plots always take the branch above.
    const auto legend = std::find_if(
        plot.colorByLegendItems.begin(), plot.colorByLegendItems.end(),
        [&](const auto &item) { return item.first == level; });
    if (legend == plot.colorByLegendItems.end()) return {};
    std::set<int> rows;
    for (const auto &[caseId, color] : plot.colorByRowColors) {
        if (color == legend->second) rows.insert(caseId);
    }
    return rows;
}

std::vector<int> SynchronizePlotColorByLinkedRows(
    std::map<int, std::string> &linkedRowColors,
    const std::map<int, std::string> &oldColorByRows,
    const std::map<int, std::string> &newColorByRows)
{
    const std::map<int, std::string> before = linkedRowColors;
    for (const auto &[row, oldColor] : oldColorByRows) {
        const auto linked = linkedRowColors.find(row);
        if (linked != linkedRowColors.end() && linked->second == oldColor) {
            linkedRowColors.erase(linked);
        }
    }
    for (const auto &[row, newColor] : newColorByRows) {
        if (row > 0 && !newColor.empty()) linkedRowColors[row] = newColor;
    }

    std::set<int> rows;
    for (const auto &[row, color] : before) {
        const auto current = linkedRowColors.find(row);
        if (current == linkedRowColors.end() || current->second != color) rows.insert(row);
    }
    for (const auto &[row, color] : linkedRowColors) {
        const auto previous = before.find(row);
        if (previous == before.end() || previous->second != color) rows.insert(row);
    }
    return std::vector<int>(rows.begin(), rows.end());
}

bool PlotColorLegendMatchesLinkedRowColors(
    const PlotModel &plot,
    const std::map<int, std::string> &linkedRowColors)
{
    if (plot.colorByVariable.empty() || plot.colorByLegendItems.empty()) return true;
    for (const auto &[row, linkedColor] : linkedRowColors) {
        const auto semantic = plot.colorByRowColors.find(row);
        if (semantic != plot.colorByRowColors.end() && semantic->second != linkedColor) {
            return false;
        }
    }
    return true;
}

std::set<int> PlotSeriesLegendRows(const PlotModel &plot,
                                   std::size_t seriesIndex)
{
    if (!PlotLinksToDataRows(plot)) return {};
    if (seriesIndex >= plot.interactionPlotLines.size()) return {};
    const InteractionPlotLine &line = plot.interactionPlotLines[seriesIndex];
    std::set<int> rows(line.caseIds.begin(), line.caseIds.end());
    if (!rows.empty()) return rows;
    for (const DataPoint &point : line.points) {
        if (point.row > 0) rows.insert(point.row);
    }
    return rows;
}

std::set<int> PlotEffectCategoryRows(const PlotModel &plot,
                                     std::size_t categoryIndex)
{
    if (categoryIndex >= plot.interactionXTickRows.size()) return {};
    const std::vector<int> &caseIds = plot.interactionXTickRows[categoryIndex];
    return std::set<int>(caseIds.begin(), caseIds.end());
}

bool PlotSeriesIntersectsSelection(const PlotModel &plot,
                                   std::size_t seriesIndex,
                                   const std::set<int> &selectedRows)
{
    if (PlotIsImputationDiagnostic(plot))
        return plot.selectedDiagnosticSeries.count(seriesIndex) != 0;
    if (selectedRows.empty()) return false;
    const std::set<int> rows = PlotSeriesLegendRows(plot, seriesIndex);
    return std::any_of(rows.begin(), rows.end(), [&](int row) {
        return selectedRows.find(row) != selectedRows.end();
    });
}

bool PlotSeriesIsFullySelected(const PlotModel &plot,
                               std::size_t seriesIndex,
                               const std::set<int> &selectedRows)
{
    if (PlotIsImputationDiagnostic(plot))
        return plot.selectedDiagnosticSeries.count(seriesIndex) != 0;
    if (selectedRows.empty()) return false;
    const std::set<int> rows = PlotSeriesLegendRows(plot, seriesIndex);
    return !rows.empty() && std::all_of(rows.begin(), rows.end(), [&](int row) {
        return selectedRows.find(row) != selectedRows.end();
    });
}

std::map<int, std::string> TimeSeriesRowColors(const PlotModel &plot)
{
    std::map<int, std::string> colors;
    if (plot.kind != "time_series") return colors;
    for (const InteractionPlotLine &series : plot.interactionPlotLines) {
        if (series.colorKey.empty()) continue;
        for (const DataPoint &point : series.points) {
            if (point.row > 0) colors.emplace(point.row, series.colorKey);
        }
    }
    return colors;
}

std::optional<std::size_t> HitPlotSeriesAtScreenPoint(
    const PlotModel &plot,
    const DataViewport &viewport,
    const Rect &plotRect,
    const Point &screenPoint,
    double maxDistance)
{
    if (!IsValidViewport(viewport) || !IsValidRect(plotRect) ||
        !std::isfinite(screenPoint.x) || !std::isfinite(screenPoint.y) ||
        !std::isfinite(maxDistance) || maxDistance < 0.0) {
        return std::nullopt;
    }
    std::optional<std::size_t> hit;
    double bestDistanceSquared = maxDistance * maxDistance;
    for (std::size_t seriesIndex = 0;
         seriesIndex < plot.interactionPlotLines.size(); ++seriesIndex) {
        const InteractionPlotLine &series = plot.interactionPlotLines[seriesIndex];
        if (series.points.empty()) continue;
        Point previous = DataToScreen(
            {series.points.front().x, series.points.front().y},
            viewport, plotRect, true);
        double distanceSquared = DistanceSquared(screenPoint, previous);
        for (std::size_t pointIndex = 1; pointIndex < series.points.size(); ++pointIndex) {
            const Point current = DataToScreen(
                {series.points[pointIndex].x, series.points[pointIndex].y},
                viewport, plotRect, true);
            const double segmentDistance = DistanceSquaredToSegment(
                screenPoint, previous, current);
            if (std::isfinite(segmentDistance)) {
                distanceSquared = std::isfinite(distanceSquared)
                    ? std::min(distanceSquared, segmentDistance)
                    : segmentDistance;
            }
            previous = current;
        }
        if (std::isfinite(distanceSquared) && distanceSquared <= bestDistanceSquared) {
            bestDistanceSquared = distanceSquared;
            hit = seriesIndex;
        }
    }
    return hit;
}

std::optional<std::size_t> HitTimeSeriesSegmentAtScreenPoint(
    const PlotModel &plot,
    const DataViewport &viewport,
    const Rect &plotRect,
    const Point &screenPoint,
    double markerRadius,
    double lineTolerance)
{
    if (plot.kind != "time_series" || !IsValidViewport(viewport) ||
        !IsValidRect(plotRect) || !std::isfinite(markerRadius) ||
        markerRadius < 0.0) return std::nullopt;
    const double markerDistanceSquared = markerRadius * markerRadius;
    for (const DataPoint &point : plot.points) {
        if (point.row <= 0) continue;
        const Point marker = DataToScreen(
            {point.x, point.y}, viewport, plotRect, true);
        if (DistanceSquared(screenPoint, marker) <= markerDistanceSquared)
            return std::nullopt;
    }
    return HitPlotSeriesAtScreenPoint(
        plot, viewport, plotRect, screenPoint, lineTolerance);
}

bool ParseTimeSeriesCellValue(const std::string &text,
                              const std::string &timeType,
                              double &value)
{
    if (ParseDataCellDouble(text, value)) return std::isfinite(value);
    if (timeType != "date" && timeType != "datetime") return false;
    int year = 0;
    unsigned month = 0;
    unsigned day = 0;
    int hour = 0;
    int minute = 0;
    double second = 0.0;
    const int matched = std::sscanf(text.c_str(), "%d-%u-%u %d:%d:%lf",
                                    &year, &month, &day, &hour, &minute, &second);
    if (matched < 3 || month < 1 || month > 12 || day < 1 || day > 31) return false;
    year -= month <= 2;
    const long long era = (year >= 0 ? year : year - 399) / 400;
    const unsigned yoe = static_cast<unsigned>(year - era * 400);
    const unsigned shiftedMonth = month > 2 ? month - 3 : month + 9;
    const unsigned doy = (153 * shiftedMonth + 2) / 5 + day - 1;
    const unsigned doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    const double days = static_cast<double>(era * 146097 + static_cast<long long>(doe) - 719468);
    value = timeType == "datetime"
        ? days * 86400.0 + hour * 3600.0 + minute * 60.0 + second
        : days;
    return std::isfinite(value);
}

std::string InferTimeSeriesTimeType(const DataColumn &column)
{
    if (NormalizeVariableType(column.type) == "datetime") {
        for (const std::string &cell : column.values) {
            if (cell.find(':') != std::string::npos) return "datetime";
        }
        return "date";
    }

    // Years are commonly stored as an ordinary numeric variable. Treat a
    // wholly integral, plausible calendar-year column as temporal so that its
    // axis uses calendar labels (2020, 2021, ...) instead of 2e+03.
    bool sawValue = false;
    for (const std::string &cell : column.values) {
        if (DataCellIsMissing(cell)) continue;
        double value = NAN;
        if (!ParseDataCellDouble(cell, value) || !std::isfinite(value) ||
            value < 1000.0 || value > 3000.0 ||
            std::fabs(value - std::round(value)) > 1.0e-9) {
            return "numeric";
        }
        sawValue = true;
    }
    return sawValue ? "year" : "numeric";
}

// Display formatting must not change which cases belong to a series.
// Match R's make.unique(..., sep = " #"), reserving original labels first.
static std::vector<std::string> TimeSeriesDisplayLabels(std::vector<std::string> labels)
{
    for (auto &label : labels) {
        std::string clean;
        bool separator = false;
        for (char c : label) {
            const bool special = c == '\r' || c == '\n' || c == '\t' || c == '|';
            if (!special) clean.push_back(c);
            else if (!separator) clean.push_back(' ');
            separator = special;
        }
        label = clean;
    }
    std::set<std::string> reserved(labels.begin(), labels.end()), used;
    for (auto &label : labels) {
        const std::string base = label;
        if (used.count(label)) {
            int suffix = 1;
            do { label = base + " #" + std::to_string(suffix++); }
            while (reserved.count(label) || used.count(label));
        }
        used.insert(label);
    }
    return labels;
}

bool RebuildTimeSeriesFromDataFrame(PlotModel &plot,
                                    const DataFrameModel &df,
                                    std::string *error)
{
    const DataColumn *timeColumn = FindDataColumnInDataFrame(df, plot.xLabel);
    const DataColumn *valueColumn = FindDataColumnInDataFrame(df, plot.yLabel);
    const DataColumn *groupColumn = plot.timeSeriesGroupVariable.empty()
        ? nullptr : FindDataColumnInDataFrame(df, plot.timeSeriesGroupVariable);
    if (!timeColumn || !valueColumn || (!plot.timeSeriesGroupVariable.empty() && !groupColumn)) {
        if (error) *error = "The selected time, value, or series variable is unavailable.";
        return false;
    }
    plot.timeSeriesTimeType = InferTimeSeriesTimeType(*timeColumn);
    plot.points.clear();
    plot.timeSeriesPointGroups.clear();
    std::map<std::string, std::size_t> groupIndices;
    std::vector<std::string> labels;
    std::vector<std::size_t> pointGroups;
    std::vector<bool> missingGroups;
    const std::size_t rowCount = std::min(timeColumn->values.size(), valueColumn->values.size());
    for (std::size_t row = 0; row < rowCount; ++row) {
        double time = NAN;
        double value = NAN;
        if (!ParseTimeSeriesCellValue(timeColumn->values[row], plot.timeSeriesTimeType, time) ||
            !ParseDataCellDouble(valueColumn->values[row], value) ||
            !std::isfinite(time) || !std::isfinite(value)) {
            continue;
        }
        plot.points.push_back(DataPoint{time, value, static_cast<int>(row + 1)});
        if (groupColumn) {
            const std::vector<std::string> &groupValues =
                groupColumn->displayValues.size() == groupColumn->values.size() && !groupColumn->displayValues.empty()
                    ? groupColumn->displayValues : groupColumn->values;
            const std::string raw = row < groupColumn->values.size() ? groupColumn->values[row] : "";
            const bool missing = DataCellIsMissing(raw);
            const std::string key = missing ? "missing:" : "value:" + raw;
            auto found = groupIndices.find(key);
            if (found == groupIndices.end()) {
                const auto index = labels.size();
                found = groupIndices.emplace(key, index).first;
                labels.push_back(missing ? "" : row < groupValues.size() ? groupValues[row] : raw);
                missingGroups.push_back(missing);
            }
            pointGroups.push_back(found->second);
        }
    }
    if (plot.points.empty()) {
        if (error) *error = "No complete numeric time/value pairs are available.";
        plot.interactionPlotLines.clear();
        return false;
    }
    const bool literalMissing = std::find(labels.begin(), labels.end(), "(missing)") != labels.end();
    for (std::size_t i = 0; i < labels.size(); ++i)
        if (missingGroups[i]) labels[i] = literalMissing ? "(missing values)" : "(missing)";
    // Real categories keep their names; the synthetic missing category comes
    // last for disambiguation, as in the R preparation path.
    std::vector<std::string> displayLabels;
    std::vector<std::size_t> displayOrder;
    for (bool missing : {false, true}) {
        for (std::size_t i = 0; i < labels.size(); ++i) {
            if (missingGroups[i] != missing) continue;
            displayOrder.push_back(i);
            displayLabels.push_back(labels[i]);
        }
    }
    displayLabels = TimeSeriesDisplayLabels(std::move(displayLabels));
    for (std::size_t i = 0; i < displayOrder.size(); ++i) labels[displayOrder[i]] = displayLabels[i];
    for (auto index : pointGroups) plot.timeSeriesPointGroups.push_back(labels[index]);
    plot.interactionPlotLines = BuildTimeSeriesLines(
        plot.points, plot.timeSeriesPointGroups, plot.yLabel);
    ComputeRanges(plot);
    return true;
}

std::string FormatTimeSeriesTick(double value, const std::string &timeType)
{
    if (timeType == "year" && std::isfinite(value)) {
        if (std::fabs(value - std::round(value)) < 1.0e-9) {
            return std::to_string(static_cast<long long>(std::llround(value)));
        }
        std::ostringstream out;
        out << std::fixed << std::setprecision(3) << value;
        std::string label = out.str();
        while (!label.empty() && label.back() == '0') label.pop_back();
        if (!label.empty() && label.back() == '.') label.pop_back();
        return label;
    }
    if (!std::isfinite(value) || (timeType != "date" && timeType != "datetime")) {
        std::ostringstream out;
        out << std::setprecision(3) << value;
        return out.str();
    }
    // Civil-date conversion from days since 1970-01-01. This is independent
    // of the machine timezone and matches R's numeric representation of Date.
    double dayValue = timeType == "datetime" ? std::floor(value / 86400.0) : std::floor(value);
    long long z = static_cast<long long>(dayValue) + 719468;
    const long long era = (z >= 0 ? z : z - 146096) / 146097;
    const unsigned doe = static_cast<unsigned>(z - era * 146097);
    const unsigned yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    long long year = static_cast<long long>(yoe) + era * 400;
    const unsigned doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    const unsigned mp = (5 * doy + 2) / 153;
    const unsigned day = doy - (153 * mp + 2) / 5 + 1;
    const unsigned month = mp < 10 ? mp + 3 : mp - 9;
    year += month <= 2;
    char buffer[40];
    if (timeType == "date") {
        std::snprintf(buffer, sizeof(buffer), "%04lld-%02u-%02u", year, month, day);
    } else {
        double secondsInDay = value - std::floor(value / 86400.0) * 86400.0;
        if (secondsInDay < 0.0) secondsInDay += 86400.0;
        const int hour = static_cast<int>(secondsInDay / 3600.0);
        const int minute = static_cast<int>(std::fmod(secondsInDay, 3600.0) / 60.0);
        std::snprintf(buffer, sizeof(buffer), "%04lld-%02u-%02u %02d:%02d",
                      year, month, day, hour, minute);
    }
    return buffer;
}

static std::string FormatTimeSeriesTickAtResolution(double value,
                                                    const std::string &timeType,
                                                    double resolution)
{
    if (timeType != "datetime" || resolution >= 60.0) {
        return FormatTimeSeriesTick(value, timeType);
    }
    const std::string minuteLabel = FormatTimeSeriesTick(value, timeType);
    double secondsInDay = value - std::floor(value / 86400.0) * 86400.0;
    if (secondsInDay < 0.0) secondsInDay += 86400.0;
    const int second = static_cast<int>(std::floor(std::fmod(secondsInDay, 60.0) + 0.5)) % 60;
    char buffer[8];
    std::snprintf(buffer, sizeof(buffer), ":%02d", second);
    return minuteLabel + buffer;
}

std::vector<TimeSeriesAxisTick> BuildScreeAxisTicks(const PlotModel &model)
{
    std::set<int> components;
    auto collect = [&](const std::vector<DataPoint> &points) {
        for (const auto &point : points) {
            if (std::isfinite(point.x) && point.x >= 1 &&
                point.x <= std::numeric_limits<int>::max() &&
                point.x == std::floor(point.x) && point.x >= model.xmin && point.x <= model.xmax)
                components.insert(static_cast<int>(point.x));
        }
    };
    collect(model.points);
    collect(model.screeParallelPoints);
    std::vector<TimeSeriesAxisTick> ticks;
    for (int component : components) ticks.push_back({static_cast<double>(component), std::to_string(component)});
    return ticks;
}

std::vector<TimeSeriesAxisTick> BuildTimeSeriesAxisTicks(
    double minimum,
    double maximum,
    const std::string &timeType,
    int targetCount)
{
    std::vector<TimeSeriesAxisTick> ticks;
    if (!std::isfinite(minimum) || !std::isfinite(maximum) || maximum < minimum ||
        (timeType != "year" && timeType != "date" && timeType != "datetime")) {
        return ticks;
    }
    const double span = maximum - minimum;
    const int count = std::max(2, targetCount);
    const std::vector<double> dateSteps = {
        1.0, 2.0, 7.0, 14.0, 30.0, 60.0, 90.0, 180.0,
        365.0, 730.0, 1825.0, 3650.0
    };
    const std::vector<double> dateTimeSteps = {
        1.0, 5.0, 15.0, 30.0, 60.0, 300.0, 900.0, 1800.0,
        3600.0, 10800.0, 21600.0, 43200.0, 86400.0, 172800.0,
        604800.0, 1209600.0, 2592000.0, 7776000.0, 15552000.0,
        31536000.0, 63072000.0, 157680000.0
    };
    const std::vector<double> yearSteps = {
        1.0, 2.0, 5.0, 10.0, 20.0, 25.0, 50.0, 100.0, 200.0, 500.0, 1000.0
    };
    const std::vector<double> &steps = timeType == "year"
        ? yearSteps : (timeType == "date" ? dateSteps : dateTimeSteps);
    double step = steps.front();
    if (span > 0.0) {
        double bestDistance = std::numeric_limits<double>::infinity();
        for (double candidate : steps) {
            const double distance = std::fabs(
                span / candidate - static_cast<double>(count - 1));
            if (distance < bestDistance) {
                bestDistance = distance;
                step = candidate;
            }
        }
    }
    const double epsilon = step * 1e-9;
    double first = std::ceil((minimum - epsilon) / step) * step;
    for (double value = first;
         value <= maximum + epsilon && ticks.size() < static_cast<std::size_t>(count + 3);
         value += step) {
        const std::string label = FormatTimeSeriesTickAtResolution(value, timeType, step);
        if (ticks.empty() || ticks.back().label != label) {
            ticks.push_back(TimeSeriesAxisTick{value, label});
        }
    }
    if (ticks.empty()) {
        double value = timeType == "date"
            ? std::floor((minimum + maximum) * 0.5 + 0.5)
            : (minimum + maximum) * 0.5;
        ticks.push_back(TimeSeriesAxisTick{
            value, FormatTimeSeriesTickAtResolution(value, timeType, step)});
    }
    return ticks;
}

std::vector<TimeSeriesLegendItem> BuildTimeSeriesLegendLayout(
    std::size_t seriesCount,
    const Rect &plotRect,
    const std::string &position,
    double rowHeight,
    double legendWidth)
{
    std::vector<TimeSeriesLegendItem> items;
    if (!IsValidRect(plotRect) || seriesCount == 0) return items;
    rowHeight = std::max(10.0, rowHeight);
    legendWidth = std::min(std::max(80.0, legendWidth), plotRect.width);
    const double margin = 10.0;
    if (position == "top" || position == "bottom") {
        const double availableWidth = std::max(1.0, plotRect.width - 2.0 * margin);
        const double minimumItemWidth = 112.0;
        const std::size_t columns = std::min(
            seriesCount, static_cast<std::size_t>(std::max(
                1.0, std::floor(availableWidth / minimumItemWidth))));
        const std::size_t rows = (seriesCount + columns - 1) / columns;
        const double itemWidth = availableWidth / static_cast<double>(columns);
        const double firstCenterY = position == "bottom"
            ? plotRect.y + plotRect.height - margin -
                rowHeight * (static_cast<double>(rows) - 0.5)
            : plotRect.y + margin + rowHeight * 0.5;
        items.reserve(seriesCount);
        for (std::size_t index = 0; index < seriesCount; ++index) {
            const std::size_t row = index / columns;
            const std::size_t column = index % columns;
            const double x = plotRect.x + margin + itemWidth * static_cast<double>(column);
            const double centerY = firstCenterY + rowHeight * static_cast<double>(row);
            items.push_back(TimeSeriesLegendItem{
                index,
                Point{x, centerY},
                Point{x + 20.0, centerY},
                Point{x + 26.0, centerY + 4.0}
            });
        }
        return items;
    }
    const bool onLeft = position == "top_left" || position == "bottom_left";
    const bool onBottom = position == "bottom_left" || position == "bottom_right";
    const bool centered = position == "left" || position == "right";
    const std::size_t rowsPerColumn = static_cast<std::size_t>(std::max(
        1.0, std::floor((plotRect.height - 2.0 * margin) / rowHeight)));
    const std::size_t columns = (seriesCount + rowsPerColumn - 1) / rowsPerColumn;
    const double totalWidth = legendWidth * static_cast<double>(columns);
    const double firstX = (onLeft || position == "left")
        ? plotRect.x + margin
        : plotRect.x + plotRect.width - totalWidth - margin;
    const std::size_t firstColumnRows = std::min(seriesCount, rowsPerColumn);
    const double blockHeight = rowHeight * static_cast<double>(firstColumnRows);
    const double y = centered
        ? plotRect.y + (plotRect.height - blockHeight) * 0.5
        : (onBottom
            ? plotRect.y + plotRect.height - margin - blockHeight
            : plotRect.y + margin);
    items.reserve(seriesCount);
    for (std::size_t index = 0; index < seriesCount; ++index) {
        const std::size_t column = index / rowsPerColumn;
        const std::size_t row = index % rowsPerColumn;
        const double x = firstX + legendWidth * static_cast<double>(column);
        const double centerY = y + rowHeight * static_cast<double>(row) + rowHeight * 0.5;
        items.push_back(TimeSeriesLegendItem{
            index,
            Point{x, centerY},
            Point{x + 20.0, centerY},
            Point{x + 26.0, centerY + 4.0}
        });
    }
    return items;
}

bool SetInteractionLegendPosition(PlotModel &model,
                                  const std::string &position)
{
    if (position != "right" && position != "left" &&
        position != "top" && position != "bottom" &&
        position != "outside_right" && position != "outside_left" &&
        position != "outside_top" && position != "outside_bottom" &&
        position != "manual") return false;
    if (position == "manual") {
        model.interactionLegendUsesCustomPosition = true;
        return true;
    }
    model.interactionLegendPosition = position;
    model.interactionLegendUsesCustomPosition = false;
    return true;
}

bool SetInteractionLegendCustomPosition(PlotModel &model,
                                        double normalizedX,
                                        double normalizedY)
{
    if (!std::isfinite(normalizedX) || !std::isfinite(normalizedY)) return false;
    model.interactionLegendUsesCustomPosition = true;
    model.interactionLegendX = std::clamp(normalizedX, -2.0, 3.0);
    model.interactionLegendY = std::clamp(normalizedY, -2.0, 3.0);
    return true;
}

static std::size_t OutsideLegendColumns(const PlotModel &model, double plotWidth)
{
    if (model.interactionPlotLines.empty()) return 1;
    std::size_t longest = 0;
    for (const auto &line : model.interactionPlotLines)
        longest = std::max(longest, line.label.size());
    const double itemWidth = std::clamp(55.0 + 6.2 * longest, 112.0, 260.0);
    return std::min(model.interactionPlotLines.size(),
        static_cast<std::size_t>(std::max(1.0,
            std::floor(std::max(1.0, plotWidth - 20.0) / itemWidth))));
}

std::vector<TimeSeriesLegendItem> BuildInteractionLegendLayout(
    const PlotModel &model,
    const Rect &plotRect,
    double rowHeight,
    double legendWidth)
{
    const std::string position = model.kind == "time_series"
        ? model.timeSeriesLegendPosition : model.interactionLegendPosition;
    auto items = BuildTimeSeriesLegendLayout(
        model.interactionPlotLines.size(), plotRect,
        position.rfind("outside_", 0) == 0 ? "left" : position, rowHeight, legendWidth);
    if (!model.interactionLegendUsesCustomPosition &&
        position.rfind("outside_", 0) == 0 && !items.empty()) {
        if (position == "outside_top" || position == "outside_bottom") {
            const std::size_t columns = OutsideLegendColumns(model, plotRect.width);
            const double itemWidth = std::max(1.0, plotRect.width - 20.0) / columns;
            const double titleExtra = 18.0 *
                (std::max<std::size_t>(1,
                    RegressionEffectTitleLines(model, plotRect.width + 128.0).size()) - 1);
            const double firstY = position == "outside_top" ? 92.0 + titleExtra
                : plotRect.y + plotRect.height + 82.0;
            for (std::size_t index = 0; index < items.size(); ++index) {
                const double x = plotRect.x + 12.0 + itemWidth * (index % columns);
                const double y = firstY + rowHeight * (index / columns);
                items[index].sampleStart = {x, y};
                items[index].sampleEnd = {x + 20.0, y};
                items[index].labelAnchor = {x + 26.0, y + 4.0};
            }
            return items;
        }
        const double firstX = position == "outside_right" ?
            plotRect.x + plotRect.width + 20.0 : position == "outside_left" ?
            plotRect.x - 20.0 - legendWidth : plotRect.x + 12.0;
        const double firstY = plotRect.y +
            std::max(24.0, (plotRect.height - rowHeight * items.size()) / 2.0);
        const double dx = firstX - items.front().sampleStart.x;
        const double dy = firstY - items.front().sampleStart.y;
        for (auto &item : items) {
            item.sampleStart.x += dx; item.sampleStart.y += dy;
            item.sampleEnd.x += dx; item.sampleEnd.y += dy;
            item.labelAnchor.x += dx; item.labelAnchor.y += dy;
        }
        return items;
    }
    if (!model.interactionLegendUsesCustomPosition || items.empty() ||
        plotRect.width <= 0.0 || plotRect.height <= 0.0) return items;

    const Point target{
        plotRect.x + model.interactionLegendX * plotRect.width,
        plotRect.y + model.interactionLegendY * plotRect.height
    };
    const double dx = target.x - items.front().sampleStart.x;
    const double dy = target.y - items.front().sampleStart.y;
    for (auto &item : items) {
        item.sampleStart.x += dx; item.sampleStart.y += dy;
        item.sampleEnd.x += dx; item.sampleEnd.y += dy;
        item.labelAnchor.x += dx; item.labelAnchor.y += dy;
    }
    return items;
}

Rect RegressionEffectPlotRect(const PlotModel &model, double width, double height)
{
    const bool effect = model.kind == "glm_interaction" || model.kind == "interaction_plot";
    const double titleExtra = effect
        ? 18.0 * (std::max<std::size_t>(1,
            RegressionEffectTitleLines(model, width).size()) - 1) : 0.0;
    Rect plot{88.0, 52.0 + titleExtra, std::max(10.0, width - 128.0),
              std::max(10.0, height - 122.0 - titleExtra)};
    if (model.kind != "glm_interaction" && model.kind != "interaction_plot") return plot;
    if (model.interactionLegendUsesCustomPosition) return plot;
    const std::string &position = model.interactionLegendPosition;
    if (position.rfind("outside_", 0) != 0) return plot;
    std::size_t longest = model.interactionLegendTitleVisible ?
        model.interactionLegendTitle.size() : 0;
    for (const auto &line : model.interactionPlotLines)
        longest = std::max(longest, line.label.size());
    const double desiredWidth = std::clamp(55.0 + 6.2 * longest, 130.0, 260.0);
    if (position == "outside_right" || position == "outside_left") {
        const double reserve = std::min(desiredWidth, std::max(0.0, plot.width - 180.0));
        if (position == "outside_left") plot.x += reserve;
        plot.width -= reserve;
    } else {
        const std::size_t columns = OutsideLegendColumns(model, plot.width);
        const std::size_t rows =
            (model.interactionPlotLines.size() + columns - 1) / columns;
        const double desiredHeight = position == "outside_bottom"
            ? 80.0 + 17.0 * rows
            : 24.0 + 17.0 * rows +
              (model.interactionLegendTitleVisible && !model.interactionLegendTitle.empty() ? 18.0 : 0.0);
        const double reserve = std::min(desiredHeight, std::max(0.0, plot.height - 125.0));
        if (position == "outside_top") plot.y += reserve;
        plot.height -= reserve;
    }
    return plot;
}

std::vector<std::string> RegressionEffectTitleLines(const PlotModel &model, double width)
{
    if (model.title.empty()) return {};
    const std::size_t capacity = static_cast<std::size_t>(std::max(
        18.0, std::floor(std::max(150.0, width - 128.0) / 7.4)));
    std::vector<std::string> lines;
    std::istringstream words(model.title);
    std::string word;
    while (words >> word) {
        while (word.size() > capacity) {
            if (!lines.empty() && !lines.back().empty()) lines.emplace_back();
            std::size_t split = capacity;
            while (split > 0 && split < word.size() &&
                   (static_cast<unsigned char>(word[split]) & 0xc0) == 0x80)
                --split;
            if (split == 0) split = capacity;
            if (lines.empty()) lines.emplace_back();
            lines.back() = word.substr(0, split);
            word.erase(0, split);
            lines.emplace_back();
        }
        if (lines.empty()) lines.emplace_back();
        if (lines.back().empty() || lines.back().size() + word.size() + 1 <= capacity) {
            if (!lines.back().empty()) lines.back() += ' ';
            lines.back() += word;
        } else {
            lines.push_back(word);
        }
    }
    return lines;
}

std::string RegressionEffectTickLabel(double value, double minimum, double maximum)
{
    if (!std::isfinite(value)) return {};
    const double step = std::fabs(maximum - minimum) / 5.0;
    int decimals = step > 0.0 && step < 1.0
        ? std::clamp(static_cast<int>(std::ceil(-std::log10(step))) + 1, 0, 6) : 0;
    std::ostringstream stream;
    stream << std::fixed << std::setprecision(decimals) << value;
    return stream.str();
}

bool RegressionEffectXAxisIsCategorical(const PlotModel &model)
{
    return model.kind == "glm_interaction" && !model.interactionXTicks.empty();
}

bool IsPooledRegressionEffectPlot(const PlotModel &model)
{
    if (model.kind != "glm_interaction" || !model.isRegressionDerivedPlot)
        return false;
    return model.regressionDerivedKind == "effect_plot" ||
        model.regressionDerivedKind == "interaction_plot";
}

double RegressionEffectSeriesDodgeOffset(std::size_t seriesIndex,
                                         std::size_t seriesCount)
{
    if (seriesCount <= 1 || seriesIndex >= seriesCount) return 0.0;
    const double spacing = std::min(10.0, 28.0 / static_cast<double>(seriesCount));
    return (static_cast<double>(seriesIndex) -
            (static_cast<double>(seriesCount) - 1.0) / 2.0) * spacing;
}

std::vector<TimeSeriesDirectLabel> BuildTimeSeriesDirectLabels(
    const std::vector<InteractionPlotLine> &lines,
    const DataViewport &viewport,
    const Rect &plotRect,
    double lineHeight)
{
    std::vector<TimeSeriesDirectLabel> labels;
    if (!IsValidRect(plotRect) || !IsValidViewport(viewport) || lines.empty()) return labels;
    lineHeight = std::max(10.0, lineHeight);
    for (std::size_t index = 0; index < lines.size(); ++index) {
        if (lines[index].points.empty()) continue;
        const DataPoint &first = lines[index].points.front();
        const Point anchor = DataToScreen(Point{first.x, first.y}, viewport, plotRect, true);
        if (!std::isfinite(anchor.x) || !std::isfinite(anchor.y)) continue;
        labels.push_back(TimeSeriesDirectLabel{
            index, lines[index].label, lines[index].colorKey, anchor, anchor, anchor
        });
    }
    std::stable_sort(labels.begin(), labels.end(), [](const TimeSeriesDirectLabel &left,
                                                      const TimeSeriesDirectLabel &right) {
        return left.seriesAnchor.y < right.seriesAnchor.y;
    });
    const double top = plotRect.y + 4.0;
    const double bottom = plotRect.y + plotRect.height - 4.0;
    double nextY = top;
    for (TimeSeriesDirectLabel &label : labels) {
        const double y = std::max(label.seriesAnchor.y, nextY);
        label.labelAnchor.y = y;
        nextY = y + lineHeight;
    }
    if (!labels.empty() && labels.back().labelAnchor.y > bottom) {
        labels.back().labelAnchor.y = bottom;
        for (std::size_t index = labels.size() - 1; index > 0; --index) {
            labels[index - 1].labelAnchor.y = std::min(
                labels[index - 1].labelAnchor.y,
                labels[index].labelAnchor.y - lineHeight);
        }
        if (labels.front().labelAnchor.y < top) {
            const double offset = top - labels.front().labelAnchor.y;
            for (TimeSeriesDirectLabel &label : labels) label.labelAnchor.y += offset;
        }
    }
    for (TimeSeriesDirectLabel &label : labels) {
        // labelAnchor.x is the right edge of the text.  Keeping it left of the
        // first observation avoids that marker when space allows; constrain
        // long labels to the plotting area so they cannot be clipped at the edge.
        const double textWidth = std::min(plotRect.width - 8.0,
            6.5 * static_cast<double>(label.label.size()));
        label.labelAnchor.x = std::clamp(label.seriesAnchor.x - 12.0,
            plotRect.x + std::max(0.0,textWidth) + 4.0,
            plotRect.x + plotRect.width - 4.0);
        label.leaderEnd = Point{label.labelAnchor.x + 4.0, label.labelAnchor.y};
    }
    std::stable_sort(labels.begin(), labels.end(), [](const TimeSeriesDirectLabel &left,
                                                      const TimeSeriesDirectLabel &right) {
        return left.seriesIndex < right.seriesIndex;
    });
    return labels;
}

NumericVariable NumericVariableFromColumn(const DataColumn &col)
{
    NumericVariable var;
    var.name = col.name;
    var.values.reserve(col.values.size());
    for (const std::string &value : col.values) {
        double parsed = NAN;
        var.values.push_back(ParseDataCellDouble(value, parsed) ? parsed : NAN);
    }
    return var;
}

bool PrepareScatterplotVariablesFromDataFrame(PlotModel &plot,
                                              const DataFrameModel &df,
                                              const std::string &xVariable,
                                              const std::string &yVariable,
                                              std::string *error)
{
    const DataColumn *xColumn = FindDataColumnInDataFrame(df, xVariable);
    const DataColumn *yColumn = FindDataColumnInDataFrame(df, yVariable);
    if (!xColumn || !yColumn) {
        if (error) *error = "The selected correlation variables are no longer available in the dataset.";
        return false;
    }

    NumericVariable x = NumericVariableFromColumn(*xColumn);
    NumericVariable y = NumericVariableFromColumn(*yColumn);
    const std::size_t rowCount = std::min(x.values.size(), y.values.size());
    bool hasPair = false;
    for (std::size_t row = 0; row < rowCount; ++row) {
        if (std::isfinite(x.values[row]) && std::isfinite(y.values[row])) {
            hasPair = true;
            break;
        }
    }
    if (!hasPair) {
        if (error) {
            *error = "The selected correlation variables do not contain paired numeric scores to plot.";
        }
        return false;
    }

    const auto removeCoordinate = [&](const std::string &name) {
        plot.variables.erase(std::remove_if(plot.variables.begin(), plot.variables.end(),
                                            [&](const NumericVariable &variable) {
                                                return variable.name == name;
                                            }),
                             plot.variables.end());
    };
    removeCoordinate(xVariable);
    removeCoordinate(yVariable);
    plot.variables.push_back(std::move(x));
    plot.variables.push_back(std::move(y));
    SetVariableMetaType(plot.variableMeta, xVariable, xColumn->type);
    SetVariableMetaType(plot.variableMeta, yVariable, yColumn->type);
    plot.xLabel = xVariable;
    plot.yLabel = yVariable;
    RebuildPointsForCurrentVariables(plot);
    ComputeRanges(plot);
    if (error) error->clear();
    return true;
}

void SyncPlotVariableFromColumn(PlotModel &plot, const DataColumn &col)
{
    SetVariableMetaType(plot.variableMeta, col.name, col.type);
    plot.variables.erase(std::remove_if(plot.variables.begin(), plot.variables.end(),
                                        [&](const NumericVariable &var) { return var.name == col.name; }),
                         plot.variables.end());
    if (col.type == "numeric") {
        plot.variables.push_back(NumericVariableFromColumn(col));
    }
    if (plot.kind == "scatter") {
        if (FindNumericVariable(plot, plot.xLabel) && FindNumericVariable(plot, plot.yLabel)) {
            RebuildPointsForCurrentVariables(plot);
        } else {
            plot.points.clear();
        }
        ComputeRanges(plot);
    } else if (plot.kind == "scatter_matrix") {
        RebuildScatterMatrixPoints(plot);
        ComputeRanges(plot);
    }
}

void SyncPlotVariablesFromDataFrame(PlotModel &plot, const DataFrameModel &df)
{
    plot.variables.clear();
    plot.variableMeta.clear();
    for (const DataColumn &col : df.columns) {
        plot.variableMeta.push_back(VariableMeta{col.name, col.type});
        if (col.type == "numeric") {
            plot.variables.push_back(NumericVariableFromColumn(col));
        }
    }
}

void RefreshDatasetSeedPlotAfterVariableSync(PlotModel &seed)
{
    if (!seed.variables.empty()) {
        if (!FindNumericVariable(seed, seed.xLabel)) {
            seed.xLabel = seed.variables.front().name;
        }
        if (!FindNumericVariable(seed, seed.yLabel)) {
            seed.yLabel = seed.variables.size() > 1 ? seed.variables[1].name : seed.variables.front().name;
        }
        RebuildPointsForCurrentVariables(seed);
        ComputeRanges(seed);
    } else {
        seed.points.clear();
        ComputeRanges(seed);
    }
}

void PopulateDatasetSeedPlot(PlotModel &seed,
                             const DataFrameModel &df,
                             const std::string &group)
{
    seed.isDatasetSeed = true;
    seed.id = "dataset_seed_" + group;
    seed.group = group;
    seed.title = group;
    seed.points.clear();
    SyncPlotVariablesFromDataFrame(seed, df);
    if (!seed.variables.empty()) {
        seed.xLabel = seed.variables[0].name;
        seed.yLabel = seed.variables.size() > 1 ? seed.variables[1].name : seed.variables[0].name;
        seed.title = seed.yLabel + " vs " + seed.xLabel;
        RefreshDatasetSeedPlotAfterVariableSync(seed);
    }
}

bool BuildComparisonSeedPlot(const DataFrameModel &df,
                             const std::string &group,
                             PlotModel &seed)
{
    seed = PlotModel();
    seed.kind = "comparison-seed";
    seed.id = "seed_" + group;
    seed.group = group;
    for (const DataColumn &col : df.columns) {
        seed.variableMeta.push_back(VariableMeta{col.name, col.type});
        if (col.type != "numeric") continue;
        NumericVariable variable;
        variable.name = col.name;
        variable.values.reserve(col.values.size());
        for (const std::string &value : col.values) {
            double parsed = NAN;
            variable.values.push_back(ParseDataCellDouble(value, parsed) ? parsed : NAN);
        }
        seed.variables.push_back(std::move(variable));
    }
    if (seed.variables.empty()) return false;
    seed.yLabel = seed.variables.front().name;
    seed.xLabel = seed.variables.size() > 1 ? seed.variables[1].name : seed.variables.front().name;
    const std::size_t rowCount = seed.variables.front().values.size();
    for (std::size_t index = 0; index < rowCount; ++index) {
        const double y = seed.variables.front().values[index];
        const double x = seed.variables.size() > 1 ? seed.variables[1].values[index]
                                                  : static_cast<double>(index + 1);
        if (std::isfinite(x) && std::isfinite(y)) {
            seed.points.push_back(DataPoint{x, y, static_cast<int>(index + 1)});
        }
    }
    ComputeRanges(seed);
    return true;
}

} // namespace core
} // namespace rlispstat
