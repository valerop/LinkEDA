#include "plot_geometry.h"
#include "string_utils.h"
#include "format_model.h"
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

bool IsValidRect(const Rect &rect)
{
    return std::isfinite(rect.x) && std::isfinite(rect.y) &&
        std::isfinite(rect.width) && std::isfinite(rect.height) &&
        rect.width > 0.0 && rect.height > 0.0;
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
                         bool useFixedBrushWhenSmall,
                         double brushWidth,
                         double brushHeight,
                         double clickThreshold)
{
    Rect rect = RectBetweenPoints(start, current);
    if (useFixedBrushWhenSmall &&
        rect.width < clickThreshold &&
        rect.height < clickThreshold) {
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

std::string TermDisplayType(const std::string &term, const std::string &type, const std::string &response)
{
    return ResolvedModelTermDisplayType(term, {}, type);
}

bool PlotLinksToDataRows(const PlotModel &model)
{
    return model.kind != "pca_scree";
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
}

void RemoveOverlaySource(PlotModel &model, const std::string &source)
{
    model.overlays.erase(
        std::remove_if(model.overlays.begin(), model.overlays.end(), [&](const Overlay &overlay) {
            return overlay.type == "lm" && overlay.source == source;
        }),
        model.overlays.end()
    );
}

SmoothCurveData PendingSmoothCurve(SmoothCurveScope scope)
{
    SmoothCurveData pending;
    pending.scope = scope;
    pending.groupId = ".";
    pending.ok = false;
    pending.message = "needed";
    return pending;
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
    return std::any_of(curves.begin(), curves.end(),
                       [&](const SmoothCurveData &curve) { return curve.scope == scope; });
}

bool MarkSmoothCurveScopePendingIfPresent(std::vector<SmoothCurveData> &curves,
                                          SmoothCurveScope scope)
{
    if (!SmoothCurveScopeIsPresent(curves, scope)) {
        return false;
    }
    // Keep the last valid curve visible while R recomputes it. Only an older
    // pending marker is replaced; the R reply atomically replaces the scope.
    curves.erase(std::remove_if(curves.begin(), curves.end(),
                                [&](const SmoothCurveData &curve) {
                                    return curve.scope == scope && !curve.ok;
                                }),
                 curves.end());
    curves.push_back(PendingSmoothCurve(scope));
    return true;
}

bool MarkExistingSmoothCurvesPending(std::vector<SmoothCurveData> &curves)
{
    const bool hasOverall = SmoothCurveScopeIsPresent(curves, SmoothCurveScope::Overall);
    const bool hasSelection = SmoothCurveScopeIsPresent(curves, SmoothCurveScope::Selection);
    const bool hasColor = SmoothCurveScopeIsPresent(curves, SmoothCurveScope::ColorGroup);
    if (!hasOverall && !hasSelection && !hasColor) {
        return false;
    }
    curves.erase(std::remove_if(curves.begin(), curves.end(),
                                [](const SmoothCurveData &curve) { return !curve.ok; }),
                 curves.end());
    if (hasOverall) curves.push_back(PendingSmoothCurve(SmoothCurveScope::Overall));
    if (hasSelection) curves.push_back(PendingSmoothCurve(SmoothCurveScope::Selection));
    if (hasColor) curves.push_back(PendingSmoothCurve(SmoothCurveScope::ColorGroup));
    return true;
}

bool ToggleSmoothCurveScopePending(std::vector<SmoothCurveData> &curves,
                                   SmoothCurveScope scope)
{
    const std::size_t oldSize = curves.size();
    curves.erase(std::remove_if(curves.begin(), curves.end(),
                                [&](const SmoothCurveData &curve) { return curve.scope == scope; }),
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
    }
    for (InteractionPlotLine &line : lines) {
        std::stable_sort(line.points.begin(), line.points.end(),
                         [](const DataPoint &left, const DataPoint &right) {
                             if (left.x != right.x) return left.x < right.x;
                             return left.row < right.row;
                         });
    }
    return lines;
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
            std::string group = row < groupValues.size() ? groupValues[row] : "";
            if (DataCellIsMissing(group)) group = "(missing)";
            plot.timeSeriesPointGroups.push_back(group);
        }
    }
    if (plot.points.empty()) {
        if (error) *error = "No complete numeric time/value pairs are available.";
        plot.interactionPlotLines.clear();
        return false;
    }
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
    const bool onLeft = position == "top_left" || position == "bottom_left";
    const bool onBottom = position == "bottom_left" || position == "bottom_right";
    const std::size_t visibleCount = std::min(
        seriesCount,
        static_cast<std::size_t>(std::max(1.0, std::floor((plotRect.height - 2.0 * margin) / rowHeight))));
    const double x = onLeft ? plotRect.x + margin : plotRect.x + plotRect.width - legendWidth - margin;
    const double blockHeight = rowHeight * static_cast<double>(visibleCount);
    const double y = onBottom
        ? plotRect.y + plotRect.height - margin - blockHeight
        : plotRect.y + margin;
    items.reserve(visibleCount);
    for (std::size_t index = 0; index < visibleCount; ++index) {
        const double centerY = y + rowHeight * static_cast<double>(index) + rowHeight * 0.5;
        items.push_back(TimeSeriesLegendItem{
            index,
            Point{x, centerY},
            Point{x + 20.0, centerY},
            Point{x + 26.0, centerY + 4.0}
        });
    }
    return items;
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
        // first observation prevents the label from covering that marker.
        label.labelAnchor.x = label.seriesAnchor.x - 12.0;
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
