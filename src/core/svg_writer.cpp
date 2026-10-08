#include "svg_writer.h"

#include "boxplot_model.h"
#include "format_model.h"
#include "scatterplot_model.h"
#include "trellis_scatterplot_model.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <iomanip>
#include <limits>
#include <numeric>
#include <set>
#include <sstream>

namespace rlispstat {
namespace core {

SvgWriter::SvgWriter(const ExportDimensions &dimensions)
    : dimensions_(dimensions)
{
    valid_ = finite(dimensions.widthPoints) && finite(dimensions.heightPoints) &&
        dimensions.widthPoints > 0.0 && dimensions.heightPoints > 0.0;
    if (!valid_) return;
    output_ << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
            << "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\""
            << number(dimensions.widthPoints) << "pt\" height=\""
            << number(dimensions.heightPoints) << "pt\" viewBox=\"0 0 "
            << number(dimensions.widthPoints) << " " << number(dimensions.heightPoints)
            << "\">\n";
}

bool SvgWriter::valid() const { return valid_; }
const ExportDimensions &SvgWriter::dimensions() const { return dimensions_; }

std::string SvgWriter::finish()
{
    if (!valid_) return std::string();
    if (!finished_) {
        while (openClips_ > 0) endClip();
        output_ << "</svg>\n";
        finished_ = true;
    }
    return output_.str();
}

bool SvgWriter::finite(double value) const { return std::isfinite(value); }

std::string SvgWriter::number(double value) const
{
    std::ostringstream stream;
    stream << std::fixed << std::setprecision(4) << value;
    std::string result = stream.str();
    while (!result.empty() && result.back() == '0') result.pop_back();
    if (!result.empty() && result.back() == '.') result.pop_back();
    if (result == "-0") result = "0";
    return result;
}

void SvgWriter::appendPaint(const SvgPaint &paint)
{
    output_ << " fill=\"" << EscapeAttribute(paint.fill) << "\""
            << " stroke=\"" << EscapeAttribute(paint.stroke) << "\""
            << " stroke-width=\"" << number(std::max(0.0, paint.strokeWidth)) << "\"";
    if (paint.opacity < 1.0) {
        output_ << " opacity=\"" << number(std::max(0.0, std::min(1.0, paint.opacity))) << "\"";
    }
    if (!paint.dashPattern.empty()) {
        output_ << " stroke-dasharray=\"";
        for (std::size_t index = 0; index < paint.dashPattern.size(); ++index) {
            if (index) output_ << " ";
            output_ << number(std::max(0.0, paint.dashPattern[index]));
        }
        output_ << "\"";
    }
}

void SvgWriter::rectangle(double x, double y, double width, double height,
                          const SvgPaint &paint, double radius)
{
    if (!valid_ || finished_ || !finite(x) || !finite(y) || !finite(width) ||
        !finite(height) || width < 0.0 || height < 0.0) return;
    output_ << "<rect x=\"" << number(x) << "\" y=\"" << number(y)
            << "\" width=\"" << number(width) << "\" height=\"" << number(height) << "\"";
    if (radius > 0.0) output_ << " rx=\"" << number(radius) << "\" ry=\"" << number(radius) << "\"";
    appendPaint(paint);
    output_ << "/>\n";
}

void SvgWriter::line(double x1, double y1, double x2, double y2,
                     const SvgPaint &paint)
{
    if (!valid_ || finished_ || !finite(x1) || !finite(y1) || !finite(x2) || !finite(y2)) return;
    output_ << "<line x1=\"" << number(x1) << "\" y1=\"" << number(y1)
            << "\" x2=\"" << number(x2) << "\" y2=\"" << number(y2) << "\"";
    appendPaint(paint);
    output_ << "/>\n";
}

void SvgWriter::ellipse(double centerX, double centerY, double radiusX, double radiusY,
                        const SvgPaint &paint)
{
    if (!valid_ || finished_ || !finite(centerX) || !finite(centerY) ||
        !finite(radiusX) || !finite(radiusY) || radiusX < 0.0 || radiusY < 0.0) return;
    output_ << "<ellipse cx=\"" << number(centerX) << "\" cy=\"" << number(centerY)
            << "\" rx=\"" << number(radiusX) << "\" ry=\"" << number(radiusY) << "\"";
    appendPaint(paint);
    output_ << "/>\n";
}

void SvgWriter::path(const std::string &commands, const SvgPaint &paint)
{
    if (!valid_ || finished_ || commands.empty()) return;
    output_ << "<path d=\"" << EscapeAttribute(commands) << "\"";
    appendPaint(paint);
    output_ << "/>\n";
}

void SvgWriter::text(double x, double y, const std::string &value,
                     const SvgTextStyle &style, double rotationDegrees)
{
    if (!valid_ || finished_ || !finite(x) || !finite(y) || !finite(rotationDegrees)) return;
    output_ << "<text x=\"" << number(x) << "\" y=\"" << number(y) << "\""
            << " fill=\"" << EscapeAttribute(style.fill) << "\""
            << " font-family=\"" << EscapeAttribute(style.fontFamily) << "\""
            << " font-size=\"" << number(style.fontSize) << "\""
            << " font-weight=\"" << style.fontWeight << "\""
            << " text-anchor=\"" << EscapeAttribute(style.anchor) << "\"";
    if (style.italic) output_ << " font-style=\"italic\"";
    if (std::fabs(rotationDegrees) > 1.0e-12) {
        output_ << " transform=\"rotate(" << number(rotationDegrees) << " "
                << number(x) << " " << number(y) << ")\"";
    }
    output_ << ">" << EscapeText(value) << "</text>\n";
}

std::string SvgWriter::beginClipRect(double x, double y, double width, double height)
{
    if (!valid_ || finished_ || !finite(x) || !finite(y) || !finite(width) ||
        !finite(height) || width < 0.0 || height < 0.0) return std::string();
    const std::string id = "rls-clip-" + std::to_string(++clipCounter_);
    output_ << "<defs><clipPath id=\"" << id << "\"><rect x=\"" << number(x)
            << "\" y=\"" << number(y) << "\" width=\"" << number(width)
            << "\" height=\"" << number(height) << "\"/></clipPath></defs>\n"
            << "<g clip-path=\"url(#" << id << ")\">\n";
    ++openClips_;
    return id;
}

void SvgWriter::endClip()
{
    if (!valid_ || finished_ || openClips_ <= 0) return;
    output_ << "</g>\n";
    --openClips_;
}

std::string SvgWriter::EscapeText(const std::string &value)
{
    std::string result;
    result.reserve(value.size());
    for (char ch : value) {
        switch (ch) {
            case '&': result += "&amp;"; break;
            case '<': result += "&lt;"; break;
            case '>': result += "&gt;"; break;
            default: result.push_back(ch); break;
        }
    }
    return result;
}

std::string SvgWriter::EscapeAttribute(const std::string &value)
{
    std::string result = EscapeText(value);
    std::string escaped;
    escaped.reserve(result.size());
    for (char ch : result) {
        if (ch == '\"') escaped += "&quot;";
        else if (ch == '\'') escaped += "&apos;";
        else escaped.push_back(ch);
    }
    return escaped;
}

namespace {

std::string SvgColor(const PlotRGBA &color)
{
    auto byte = [](double value) {
        return static_cast<int>(std::round(std::max(0.0, std::min(1.0, value)) * 255.0));
    };
    std::ostringstream out;
    out << '#' << std::hex << std::setfill('0')
        << std::setw(2) << byte(color.r) << std::setw(2) << byte(color.g)
        << std::setw(2) << byte(color.b);
    return out.str();
}

DataViewport SnapshotViewport(const PlotModel &model)
{
    DataViewport viewport{model.xmin, model.xmax, model.ymin, model.ymax};
    if (!IsValidViewport(viewport)) {
        std::vector<Point> points;
        points.reserve(model.points.size());
        for (const DataPoint &point : model.points)
            if (std::isfinite(point.x) && std::isfinite(point.y)) points.push_back({point.x, point.y});
        viewport = DataViewportForPoints(points, 0.06);
    }
    if (!IsValidViewport(viewport)) viewport = {0.0, 1.0, 0.0, 1.0};
    return viewport;
}

std::string SnapshotAxisTickLabel(double value)
{
    if (!std::isfinite(value)) return std::string();
    std::ostringstream stream;
    stream << std::setprecision(2) << std::defaultfloat << value;
    return stream.str();
}

void DrawSnapshotAxes(SvgWriter &writer, const PlotModel &model, const Rect &plot,
                      const PlotThemeStyleSpec &theme)
{
    const bool zeroBasedBars = model.kind == "barplot" || model.kind == "histogram";
    const Rect verticalScale = zeroBasedBars ? ZeroBaselineContentRect(plot) : plot;
    const bool screePlot = model.kind == "pca_scree";
    const auto componentTicks = screePlot ? BuildScreeAxisTicks(model) : std::vector<TimeSeriesAxisTick>();
    const bool effectPlot = model.kind == "glm_interaction" ||
        model.kind == "interaction_plot";
    SvgPaint minorGrid{"none", SvgColor(theme.minorGrid), 0.5,
                       theme.minorGrid.a, {}};
    SvgPaint grid{"none", SvgColor(theme.majorGrid), 0.7, theme.majorGrid.a, {}};
    for (int tick = 0; tick <= 5; ++tick) {
        const double fraction = static_cast<double>(tick) / 5.0;
        writer.line(plot.x, verticalScale.y + fraction * verticalScale.height,
                    plot.x + plot.width, verticalScale.y + fraction * verticalScale.height, grid);
        if (!screePlot && !(effectPlot && model.interactionXTicks.size() <= 3))
            writer.line(plot.x + fraction * plot.width, plot.y,
                    plot.x + fraction * plot.width, plot.y + plot.height, grid);
        if (theme.showMinorGrid && tick < 5) {
            const double midpoint = (static_cast<double>(tick) + 0.5) / 5.0;
            writer.line(plot.x, verticalScale.y + midpoint * verticalScale.height,
                        plot.x + plot.width, verticalScale.y + midpoint * verticalScale.height,
                        minorGrid);
            writer.line(plot.x + midpoint * plot.width, plot.y,
                        plot.x + midpoint * plot.width,
                        plot.y + plot.height, minorGrid);
        }
    }
    if (screePlot && model.xmax > model.xmin) for (const auto &tick : componentTicks) {
        const double x = plot.x + (tick.value - model.xmin) / (model.xmax - model.xmin) * plot.width;
        writer.line(x, plot.y, x, plot.y + plot.height, grid);
    }
    SvgPaint axis{"none", SvgColor(theme.axis), 1.0, theme.axis.a, {}};
    writer.line(plot.x, verticalScale.y + verticalScale.height,
                plot.x + plot.width, verticalScale.y + verticalScale.height, axis);
    writer.line(plot.x, plot.y, plot.x, plot.y + plot.height, axis);
    if (theme.showAxisTickMarks && model.kind != "histogram") {
        for (int tick = 0; tick <= 5; ++tick) {
            const double y = verticalScale.y + verticalScale.height * tick / 5.0;
            writer.line(plot.x - 4.0, y, plot.x, y, axis);
        }
        auto addXTick = [&](double x) {
            writer.line(x, verticalScale.y + verticalScale.height,
                        x, verticalScale.y + verticalScale.height + 4.0, axis);
        };
        if (screePlot && model.xmax > model.xmin) {
            for (const auto &tick : componentTicks)
                addXTick(plot.x + (tick.value - model.xmin) /
                    (model.xmax - model.xmin) * plot.width);
        } else if (!model.interactionXTicks.empty() && model.xmax > model.xmin) {
            for (const auto &tick : model.interactionXTicks)
                if (std::isfinite(tick.first))
                    addXTick(plot.x + (tick.first - model.xmin) /
                        (model.xmax - model.xmin) * plot.width);
        } else {
            for (int tick = 0; tick <= 5; ++tick)
                addXTick(plot.x + plot.width * tick / 5.0);
        }
    }
    if (model.kind == "histogram") {
        SvgTextStyle tickText{SvgColor(theme.mutedText),
                              "Arial, Helvetica, sans-serif", 9.5, 400, false, "middle"};
        int maximumCount = 1;
        for (const HistogramBin &bin : model.histogramBins)
            maximumCount = std::max(maximumCount, static_cast<int>(bin.rows.size()));
        constexpr int intervals = 4;
        for (int tick = 0; tick <= intervals; ++tick) {
            const double fraction = static_cast<double>(tick) / intervals;
            const double y = ZeroBaselineY(plot, fraction);
            if (model.histogramShowTickMarks)
                writer.line(plot.x - 4.0, y, plot.x, y, axis);
            if (model.histogramShowTickLabels) {
                SvgTextStyle yTickText = tickText;
                yTickText.anchor = "end";
                writer.text(plot.x - 7.0, y + 3.5,
                    FormatDouble(maximumCount * fraction, 1), yTickText);
            }
        }
        if (!model.histogramBins.empty()) {
            const double minimum = model.histogramBins.front().lower;
            const double maximum = model.histogramBins.back().upper;
            for (int tick = 0; tick <= intervals; ++tick) {
                const double fraction = static_cast<double>(tick) / intervals;
                const double x = verticalScale.x + verticalScale.width * fraction;
                if (model.histogramShowTickMarks)
                    writer.line(x, verticalScale.y + verticalScale.height,
                        x, verticalScale.y + verticalScale.height + 4.0, axis);
                if (model.histogramShowTickLabels)
                    writer.text(x, plot.y + plot.height + 16.0,
                        FormatDouble(minimum + (maximum - minimum) * fraction, 3),
                        tickText);
            }
        }
    } else if (effectPlot || screePlot) {
        const DataViewport viewport = SnapshotViewport(model);
        SvgTextStyle tickText{SvgColor(theme.mutedText),
                              "Arial, Helvetica, sans-serif", 10.0, 400,
                              false, "middle"};
        SvgTextStyle yTickText = tickText;
        yTickText.anchor = "end";
        for (int tick = 0; tick <= 5; ++tick) {
            const double fraction = static_cast<double>(tick) / 5.0;
            const double value = viewport.ymin +
                (viewport.ymax - viewport.ymin) * fraction;
            const double y = plot.y + plot.height * (1.0 - fraction);
            writer.text(plot.x - 6.0, y + 3.5,
                        effectPlot ? RegressionEffectTickLabel(
                            value, viewport.ymin, viewport.ymax)
                            : SnapshotAxisTickLabel(value), yTickText);
        }
        if (screePlot && model.xmax > model.xmin) {
            for (const auto &tick : componentTicks) {
                const double x = plot.x + (tick.value - model.xmin) / (model.xmax - model.xmin) * plot.width;
                writer.text(x, plot.y + plot.height + 17.0, tick.label, tickText);
            }
        } else if (!model.interactionXTicks.empty()) {
            for (const auto &tick : model.interactionXTicks) {
                if (!std::isfinite(tick.first) || viewport.xmax == viewport.xmin)
                    continue;
                const double x = plot.x +
                    (tick.first - viewport.xmin) /
                    (viewport.xmax - viewport.xmin) * plot.width;
                writer.text(x, plot.y + plot.height + 17.0,
                            tick.second, tickText);
            }
        } else {
            for (int tick = 0; tick <= 5; ++tick) {
                const double fraction = static_cast<double>(tick) / 5.0;
                const double value = viewport.xmin +
                    (viewport.xmax - viewport.xmin) * fraction;
                writer.text(plot.x + plot.width * fraction,
                            plot.y + plot.height + 17.0,
                            SnapshotAxisTickLabel(value), tickText);
            }
        }
    }
    SvgTextStyle labels;
    labels.fill = SvgColor(theme.text);
    labels.fontSize = 12.0;
    labels.anchor = "middle";
    writer.text(plot.x + plot.width / 2.0, plot.y + plot.height + 42.0,
                effectPlot && !model.presentationXLabel.empty()
                    ? model.presentationXLabel : model.xLabel, labels);
    writer.text(20.0, plot.y + plot.height / 2.0,
                effectPlot && !model.presentationYLabel.empty()
                    ? model.presentationYLabel : model.yLabel, labels, -90.0);
}

std::string SvgPolylinePath(const std::vector<Point> &points);

void DrawSnapshotPointCloud(SvgWriter &writer, const PlotModel &model, const Rect &plot,
                            const PlotThemeStyleSpec &theme)
{
    const DataViewport viewport = SnapshotViewport(model);
    SvgPaint line{"none", SvgColor(theme.markStroke), 1.2, theme.markStroke.a, {}};
    const bool effectPlot = model.kind == "glm_interaction" ||
        model.kind == "interaction_plot";
    if (model.kind == "time_series" || model.kind == "line" ||
        !model.interactionPlotLines.empty()) {
        if (!model.interactionPlotLines.empty()) {
            const bool categoricalEffectAxis =
                RegressionEffectXAxisIsCategorical(model);
            writer.beginClipRect(plot.x, plot.y, plot.width, plot.height);
            if (effectPlot &&
                model.regressionBinaryProbability &&
                model.regressionEffectQuantity == "probability_difference") {
                const Point left = DataToScreen({model.xmin, 0.0}, viewport, plot, true);
                const Point right = DataToScreen({model.xmax, 0.0}, viewport, plot, true);
                writer.line(left.x, left.y, right.x, right.y,
                    {"none", SvgColor(theme.mutedText), 1.0,
                     theme.mutedText.a, {5.0, 4.0}});
            }
            for (std::size_t seriesIndex = 0;
                 seriesIndex < model.interactionPlotLines.size(); ++seriesIndex) {
                const InteractionPlotLine &series = model.interactionPlotLines[seriesIndex];
                const double seriesOffset = categoricalEffectAxis
                    ? RegressionEffectSeriesDodgeOffset(
                        seriesIndex, model.interactionPlotLines.size())
                    : 0.0;
                SvgPaint seriesLine = line;
                PlotRGBA seriesColor = PlotColorForNameOrHex(
                    series.colorKey.empty() ? "black" : series.colorKey);
                if (effectPlot) {
                    seriesLine.stroke = SvgColor(seriesColor);
                    seriesLine.opacity = seriesColor.a;
                    seriesLine.strokeWidth = 2.2;
                    if (model.regressionConfidenceIntervalsVisible &&
                        series.confidenceLower.size() == series.points.size() &&
                        series.confidenceUpper.size() == series.points.size() &&
                        !series.confidenceLower.empty()) {
                        if (categoricalEffectAxis) {
                            SvgPaint interval{"none", SvgColor(seriesColor), 1.2,
                                              seriesColor.a, {}};
                            constexpr double cap = 4.0;
                            for (std::size_t pointIndex = 0;
                                 pointIndex < series.points.size(); ++pointIndex) {
                                Point lower = DataToScreen(
                                    {series.confidenceLower[pointIndex].x,
                                     series.confidenceLower[pointIndex].y},
                                    viewport, plot, true);
                                Point upper = DataToScreen(
                                    {series.confidenceUpper[pointIndex].x,
                                     series.confidenceUpper[pointIndex].y},
                                    viewport, plot, true);
                                lower.x += seriesOffset;
                                upper.x += seriesOffset;
                                writer.line(lower.x, lower.y, upper.x, upper.y, interval);
                                writer.line(lower.x - cap, lower.y,
                                            lower.x + cap, lower.y, interval);
                                writer.line(upper.x - cap, upper.y,
                                            upper.x + cap, upper.y, interval);
                            }
                        } else {
                            std::string bandPath;
                            for (const DataPoint &point : series.confidenceUpper) {
                                Point screen = DataToScreen({point.x, point.y}, viewport, plot, true);
                                bandPath += (bandPath.empty() ? "M " : " L ") +
                                    std::to_string(screen.x) + " " + std::to_string(screen.y);
                            }
                            for (auto point = series.confidenceLower.rbegin();
                                 point != series.confidenceLower.rend(); ++point) {
                                Point screen = DataToScreen({point->x, point->y}, viewport, plot, true);
                                bandPath += " L " + std::to_string(screen.x) + " " +
                                    std::to_string(screen.y);
                            }
                            bandPath += " Z";
                            writer.path(bandPath, {SvgColor(seriesColor), "none", 0.0, 0.16, {}});
                        }
                    }
                }
                std::string path;
                for (const DataPoint &point : series.points) {
                    Point screen = DataToScreen({point.x, point.y}, viewport, plot, true);
                    screen.x += seriesOffset;
                    path += (path.empty() ? "M " : " L ") + std::to_string(screen.x) + " " + std::to_string(screen.y);
                }
                if (!effectPlot ||
                    model.regressionConnectEstimates)
                    writer.path(path, seriesLine);
                if (effectPlot) {
                    SvgPaint pointPaint{SvgColor(seriesColor), SvgColor(seriesColor),
                                        0.8, seriesColor.a, {}};
                    for (const DataPoint &point : series.points) {
                        Point screen = DataToScreen({point.x, point.y}, viewport, plot, true);
                        screen.x += seriesOffset;
                        writer.ellipse(screen.x, screen.y, 3.0, 3.0, pointPaint);
                    }
                }
            }
            writer.endClip();
            if (effectPlot || (model.kind == "time_series" &&
                !model.timeSeriesGroupVariable.empty() && model.timeSeriesIdentification == "legend")) {
                if (model.regressionConfidenceIntervalsVisible &&
                    model.regressionConfidenceLevelVisible &&
                    PlotHasConfidenceIntervals(model)) {
                    SvgTextStyle confidenceText{SvgColor(theme.mutedText),
                        "Arial, Helvetica, sans-serif", 9.0, 400, false, "start"};
                    writer.text(plot.x + 6.0, plot.y + 14.0,
                        std::to_string(static_cast<int>(std::lround(
                            model.regressionConfidenceLevel * 100.0))) +
                            "% confidence interval",
                        confidenceText);
                }
                SvgTextStyle legendText{SvgColor(theme.text),
                    "Arial, Helvetica, sans-serif", 10.0, 400, false, "start"};
                const auto legend = BuildInteractionLegendLayout(model, plot);
                if (!legend.empty()) {
                    double left = std::numeric_limits<double>::infinity();
                    double top = std::numeric_limits<double>::infinity();
                    double right = -std::numeric_limits<double>::infinity();
                    double bottom = -std::numeric_limits<double>::infinity();
                    for (const TimeSeriesLegendItem &item : legend) {
                        const InteractionPlotLine &series =
                            model.interactionPlotLines[item.seriesIndex];
                        const double labelWidth = 6.2 *
                            static_cast<double>(series.label.size());
                        left = std::min(left, item.sampleStart.x - 6.0);
                        right = std::max(right, item.labelAnchor.x + labelWidth + 6.0);
                        top = std::min(top, item.sampleStart.y - 11.0);
                        bottom = std::max(bottom, item.sampleStart.y + 11.0);
                    }
                    const bool titled = effectPlot && model.interactionLegendTitleVisible &&
                        !model.interactionLegendTitle.empty();
                    if (titled) {
                        right = std::max(right, left +
                            6.2 * static_cast<double>(model.interactionLegendTitle.size()) + 12.0);
                        top -= 18.0;
                    }
                    writer.rectangle(left, top, right - left, bottom - top,
                        {SvgColor(theme.panel), SvgColor(theme.majorGrid), 0.6, 0.96, {}}, 4.0);
                    if (titled) writer.text(left + 6.0, top + 12.0,
                        model.interactionLegendTitle,
                        {SvgColor(theme.text), "Arial, Helvetica, sans-serif", 10.0, 700, false, "start"});
                }
                for (const TimeSeriesLegendItem &item : legend) {
                    if (item.seriesIndex >= model.interactionPlotLines.size()) continue;
                    const InteractionPlotLine &series =
                        model.interactionPlotLines[item.seriesIndex];
                    const PlotRGBA color = PlotColorForNameOrHex(
                        series.colorKey.empty() ? "black" : series.colorKey);
                    const bool lineKey = model.regressionConnectEstimates &&
                        series.points.size() >= 2;
                    if (lineKey) {
                        SvgPaint sample{"none", SvgColor(color), 2.2, color.a, {}};
                        writer.line(item.sampleStart.x, item.sampleStart.y,
                                    item.sampleEnd.x, item.sampleEnd.y, sample);
                    }
                    if (effectPlot || !lineKey) {
                        writer.ellipse((item.sampleStart.x + item.sampleEnd.x) * 0.5,
                            item.sampleStart.y, 3.0, 3.0,
                            {SvgColor(color), "none", 0.0, color.a, {}});
                    }
                    writer.text(item.labelAnchor.x, item.labelAnchor.y,
                                series.label, legendText);
                }
            }
            if (model.kind == "time_series" && !model.timeSeriesGroupVariable.empty() &&
                model.timeSeriesIdentification == "start_labels") {
                for (const auto &item : BuildTimeSeriesDirectLabels(model.interactionPlotLines, viewport, plot)) {
                    const auto color = SvgColor(PlotColorForNameOrHex(item.colorKey));
                    writer.line(item.seriesAnchor.x,item.seriesAnchor.y,item.leaderEnd.x,item.leaderEnd.y,
                        {"none",color,1.0,0.72,{}});
                    writer.text(item.labelAnchor.x,item.labelAnchor.y,item.label,
                        {color,"Arial, Helvetica, sans-serif",10.0,400,false,"end"});
                }
            }
        } else {
            std::string path;
            for (const DataPoint &point : model.points) {
                Point screen = DataToScreen({point.x, point.y}, viewport, plot, true);
                path += (path.empty() ? "M " : " L ") + std::to_string(screen.x) + " " + std::to_string(screen.y);
            }
            writer.path(path, line);
        }
    }
    const auto fittedCurves = BuildSmoothCurveDrawItems(
        model.smoothCurves, viewport, plot,
        model.scatterFitConfidenceIntervalsVisible,
        model.scatterSmoothConfidenceIntervalsVisible);
    for (const ScatterplotSmoothCurveDrawItem &curve : fittedCurves) {
        if (curve.confidencePolygon.size() < 3) continue;
        const PlotRGBA color = curve.colorName.empty()
            ? theme.smooth : PlotColorForNameOrHex(curve.colorName);
        writer.path(SvgPolylinePath(curve.confidencePolygon) + " Z",
                    {SvgColor(color), "none", 0.0, curve.confidenceAlpha, {}});
    }
    const double pointAlpha = model.scatterShadeOverlap ? 0.35 : 0.72;
    SvgPaint mark{SvgColor(PlotMarkColor(theme.markFill, theme.panel, model.scatterShadeOverlap, pointAlpha)),
                  SvgColor(PlotMarkColor(theme.markStroke, theme.panel, model.scatterShadeOverlap, pointAlpha)),
                  0.8, model.scatterShadeOverlap ? pointAlpha : 1.0, {}};
    std::map<std::pair<double, double>, std::size_t> overlapCounts;
    if (model.scatterSizeByOverlap) for (const auto &point : model.points)
        if (std::isfinite(point.x) && std::isfinite(point.y)) ++overlapCounts[{point.x, point.y}];
    std::vector<std::size_t> visualCounts;
    if (model.scatterSizeByOverlap && model.kind == "pca_biplot") {
        std::vector<Point> positions;
        for (const auto &point : model.points)
            positions.push_back(DataToScreen({point.x, point.y}, viewport, plot, true));
        visualCounts = ScatterplotVisualOverlapCounts(positions);
    }
    std::size_t pointIndex = 0;
    SvgTextStyle pointLabel{SvgColor(theme.text), "Arial, Helvetica, sans-serif",
                            9.0, 400, false, "start"};
    if (!model.imputationDiagnosticSimplified)
    for (const DataPoint &point : model.points) {
        const auto index = pointIndex++;
        if (!std::isfinite(point.x) || !std::isfinite(point.y)) continue;
        Point screen = DataToScreen({point.x, point.y}, viewport, plot, true);
        SvgPaint pointPaint = mark;
        auto color = model.frozenRowColors.find(point.row);
        if (color != model.frozenRowColors.end() && !color->second.empty()) {
            const PlotRGBA explicitColor = PlotColorForNameOrHex(color->second);
            const auto tone = PlotMarkColor(explicitColor, theme.panel, model.scatterShadeOverlap, pointAlpha);
            pointPaint.fill = SvgColor(tone);
            pointPaint.stroke = SvgColor(tone);
        }
        const double overlapScale = model.scatterSizeByOverlap
            ? std::sqrt(static_cast<double>(visualCounts.empty() ? overlapCounts.at({point.x, point.y}) : visualCounts[index])) : 1.0;
        const double radius = 3.0 * ClampPointSizeScale(model.pointSizeScale) * overlapScale;
        writer.ellipse(screen.x, screen.y, radius, radius, pointPaint);
        if (model.labelDisplayMode == "all") {
            auto label = model.frozenRowLabels.find(point.row);
            if (label != model.frozenRowLabels.end())
                writer.text(screen.x + 5.0, screen.y - 4.0, label->second, pointLabel);
        }
    }
    for (const ScatterplotSmoothCurveDrawItem &curve : fittedCurves) {
        const PlotRGBA color = curve.colorName.empty()
            ? theme.smooth : PlotColorForNameOrHex(curve.colorName);
        SvgPaint smooth{"none", SvgColor(color), curve.lineWidth,
                        curve.alpha, curve.dashed ? std::vector<double>{4.0, 3.0}
                                                  : std::vector<double>{}};
        std::string path;
        for (const Point &point : curve.points) {
            path += (path.empty() ? "M " : " L ") + std::to_string(point.x) + " " +
                    std::to_string(point.y);
        }
        writer.path(path, smooth);
    }
}

void DrawSnapshotHistogram(SvgWriter &writer, const PlotModel &model, const Rect &plot,
                           const PlotThemeStyleSpec &theme)
{
    const Rect content = ZeroBaselineContentRect(plot);
    int maximum = 1;
    for (const HistogramBin &bin : model.histogramBins)
        maximum = std::max(maximum, static_cast<int>(bin.rows.size()));
    const double width = model.histogramBins.empty() ? content.width :
        content.width / static_cast<double>(model.histogramBins.size());
    SvgPaint paint{SvgColor(theme.geomFill), SvgColor(PlotThemeWhite(0.0)), 1.0,
                   theme.geomFill.a, {}};
    for (std::size_t index = 0; index < model.histogramBins.size(); ++index) {
        const double height = content.height * static_cast<double>(model.histogramBins[index].rows.size()) /
            static_cast<double>(maximum);
        writer.rectangle(content.x + index * width + 1.0, content.y + content.height - height,
                         std::max(0.0, width - 2.0), height, paint);
    }
}

void DrawSnapshotBars(SvgWriter &writer, const PlotModel &model, const Rect &plot,
                      const PlotThemeStyleSpec &theme)
{
    const Rect content = ZeroBaselineContentRect(plot);
    int maximum = 1;
    for (const BarplotBin &bin : model.barplotBins) maximum = std::max(maximum, bin.n);
    const double slot = model.barplotBins.empty() ? content.width :
        content.width / static_cast<double>(model.barplotBins.size());
    SvgPaint paint{SvgColor(theme.geomFill), SvgColor(theme.geomStroke), 1.0,
                   theme.geomFill.a, {}};
    SvgTextStyle labels{SvgColor(theme.text), "Arial, Helvetica, sans-serif", 10.0, 400, false, "middle"};
    for (std::size_t index = 0; index < model.barplotBins.size(); ++index) {
        const BarplotBin &bin = model.barplotBins[index];
        const double height = content.height * static_cast<double>(bin.n) / static_cast<double>(maximum);
        writer.rectangle(content.x + index * slot + slot * 0.12, content.y + content.height - height,
                         slot * 0.76, height, paint);
        writer.text(content.x + (index + 0.5) * slot, plot.y + plot.height + 17.0,
                    bin.category, labels);
    }
}

std::string SvgPolylinePath(const std::vector<Point> &points);

void DrawSnapshotBoxes(SvgWriter &writer, const PlotModel &model, const Rect &plot,
                       const PlotThemeStyleSpec &theme)
{
    if (BoxplotUsesVariableAxes(model)) {
        const std::vector<BoxplotCase> cases = BoxplotCasesForModel(model);
        const BoxplotDisplayRange displayRange = BoxplotDisplayRangeForCases(
            cases, model.boxplotShowH0Simulation, model.boxplotH0Values,
            model.boxplotH0ReferenceLines, model.boxplotH0Lower, model.boxplotH0Upper);
        BoxplotLayout layout;
        layout.plotRect = plot;
        layout.categories = OrderedBoxplotCategories(
            model.boxplotDefinedCategories, cases, model.boxplotGroupOrder);
        layout.yMinimum = displayRange.minimum;
        layout.yMaximum = displayRange.maximum;
        layout.variableAxes = true;
        layout.connectRows = model.boxplotConnectRows;
        if (!IsValidBoxplotLayout(layout)) return;

        const SvgPaint grid{"none", SvgColor(theme.majorGrid), 0.8,
                            theme.majorGrid.a, {}};
        const SvgPaint axis{"none", SvgColor(theme.axis), 1.2, theme.axis.a, {}};
        const SvgTextStyle yTickStyle{SvgColor(theme.mutedText),
            "Arial, Helvetica, sans-serif", 10.0, 400, false, "end"};
        const SvgTextStyle categoryStyle{SvgColor(theme.mutedText),
            "Arial, Helvetica, sans-serif", 10.0, 400, false, "middle"};
        const SvgTextStyle axisLabelStyle{SvgColor(theme.text),
            "Arial, Helvetica, sans-serif", 11.0, 400, false, "middle"};
        for (const BoxplotAxisTick &tick :
             BoxplotYAxisTicks(layout.yMinimum, layout.yMaximum, 5)) {
            const double fraction = (tick.value - layout.yMinimum) /
                std::max(1.0e-12, layout.yMaximum - layout.yMinimum);
            const double y = plot.y + plot.height - fraction * plot.height;
            writer.line(plot.x, y, plot.x + plot.width, y, grid);
            writer.text(plot.x - 8.0, y + 3.0, tick.label, yTickStyle);
        }
        writer.line(plot.x, plot.y, plot.x, plot.y + plot.height, axis);
        writer.line(plot.x, plot.y + plot.height,
                    plot.x + plot.width, plot.y + plot.height, axis);
        for (const std::string &category : layout.categories) {
            writer.text(BoxplotCategoryCenter(layout, category),
                        plot.y + plot.height + 17.0, category, categoryStyle);
        }
        writer.text(plot.x - 50.0, plot.y + plot.height / 2.0,
                    model.yLabel, axisLabelStyle, -90.0);

        const SvgPaint boxOutline{"none", SvgColor(theme.geomStroke), 1.2,
                                  theme.geomStroke.a, {}};
        const SvgPaint boxFill{SvgColor(theme.geomFill), SvgColor(theme.geomStroke),
                               1.4, theme.geomFill.a, {}};
        const SvgPaint medianPaint{"none", SvgColor(theme.accent), 2.0,
                                   theme.accent.a, {}};
        for (const BoxplotStatsRenderItem &item :
             BuildBoxplotStatsRenderPlan(layout, BoxplotStatsForModel(model))) {
            if (model.boxplotShowWhiskers) {
                writer.line(item.upperWhiskerStart.x, item.upperWhiskerStart.y,
                            item.upperWhiskerEnd.x, item.upperWhiskerEnd.y, boxOutline);
                writer.line(item.lowerWhiskerStart.x, item.lowerWhiskerStart.y,
                            item.lowerWhiskerEnd.x, item.lowerWhiskerEnd.y, boxOutline);
                writer.line(item.upperCapStart.x, item.upperCapStart.y,
                            item.upperCapEnd.x, item.upperCapEnd.y, boxOutline);
                writer.line(item.lowerCapStart.x, item.lowerCapStart.y,
                            item.lowerCapEnd.x, item.lowerCapEnd.y, boxOutline);
            }
            if (model.boxplotShowBox) {
                writer.rectangle(item.boxRect.x, item.boxRect.y, item.boxRect.width,
                                 item.boxRect.height, boxFill);
                writer.line(item.medianStart.x, item.medianStart.y,
                            item.medianEnd.x, item.medianEnd.y, medianPaint);
            }
        }

        const std::set<CaseId> selection;
        const std::vector<BoxplotCaseGeometry> geometry =
            BoxplotCaseGeometryForLayout(layout, cases);
        const std::vector<BoxplotConnectionLine> connections =
            BuildBoxplotConnectionLinePlan(geometry, layout.categories, selection,
                                           true, model.boxplotConnectRows);
        writer.beginClipRect(plot.x, plot.y, plot.width, plot.height);
        for (const BoxplotConnectionLine &connection : connections) {
            const auto found = model.frozenRowColors.find(connection.caseId);
            PlotRGBA color = theme.markStroke;
            double alpha = 0.42;
            if (found != model.frozenRowColors.end()) {
                if (const auto palette = FindPaletteColor(found->second)) {
                    color = {palette->lightR, palette->lightG, palette->lightB, 0.96};
                } else {
                    color = PlotColorForNameOrHex(found->second, 0.96);
                }
                alpha = 0.96;
            }
            writer.path(SvgPolylinePath(connection.points),
                        {"none", SvgColor(color),
                         std::max(0.5, model.boxplotConnectionLineWidth), alpha, {}});
        }
        if (model.boxplotShowPoints) {
            for (const BoxplotPointDrawItem &point :
                 BuildBoxplotPointDrawPlan(geometry, selection)) {
                const auto found = model.frozenRowColors.find(point.caseId);
                const bool explicitColor = found != model.frozenRowColors.end();
                const PlotRGBA fill = explicitColor
                    ? PlotColorForNameOrHex(found->second) : theme.markFill;
                const PlotRGBA stroke = explicitColor ? fill : theme.markStroke;
                const double radius = 3.2 * ClampPointSizeScale(model.pointSizeScale);
                writer.ellipse(point.point.x, point.point.y, radius, radius,
                    {SvgColor(fill), SvgColor(stroke), 0.8, 0.72, {}});
            }
        }
        writer.endClip();
        return;
    }

    const std::vector<BoxplotStats> stats = BoxplotStatsForModel(model);
    if (stats.empty()) return;
    double ymin = model.ymin, ymax = model.ymax;
    if (!(ymax > ymin)) { ymin = stats.front().lower; ymax = stats.front().upper; }
    for (const BoxplotStats &stat : stats) {
        ymin = std::min(ymin, stat.lower); ymax = std::max(ymax, stat.upper);
    }
    if (!(ymax > ymin)) ymax = ymin + 1.0;
    BoxplotLayout layout;
    layout.plotRect = plot;
    layout.categories = OrderedBoxplotCategories(
        model.boxplotDefinedCategories, BoxplotCasesForModel(model), model.boxplotGroupOrder);
    layout.yMinimum = ymin;
    layout.yMaximum = ymax;
    layout.categoryLevels = BoxplotCategoryLevelsForCategories(model, layout.categories);
    SvgPaint outline{"none", SvgColor(theme.geomStroke), 1.2, theme.geomStroke.a, {}};
    SvgPaint fill{SvgColor(theme.geomFill), SvgColor(theme.geomStroke), 1.4, theme.geomFill.a, {}};
    SvgPaint median{"none", SvgColor(theme.accent), 2.0, theme.accent.a, {}};
    SvgTextStyle labels{SvgColor(theme.text), "Arial, Helvetica, sans-serif", 10.0, 400, false, "middle"};
    for (const BoxplotStatsRenderItem &item : BuildBoxplotStatsRenderPlan(layout, stats)) {
        if (model.boxplotShowWhiskers) {
            writer.line(item.upperWhiskerStart.x, item.upperWhiskerStart.y,
                        item.upperWhiskerEnd.x, item.upperWhiskerEnd.y, outline);
            writer.line(item.lowerWhiskerStart.x, item.lowerWhiskerStart.y,
                        item.lowerWhiskerEnd.x, item.lowerWhiskerEnd.y, outline);
            writer.line(item.upperCapStart.x, item.upperCapStart.y,
                        item.upperCapEnd.x, item.upperCapEnd.y, outline);
            writer.line(item.lowerCapStart.x, item.lowerCapStart.y,
                        item.lowerCapEnd.x, item.lowerCapEnd.y, outline);
        }
        if (model.boxplotShowBox) {
            writer.rectangle(item.boxRect.x, item.boxRect.y, item.boxRect.width,
                             item.boxRect.height, fill);
            writer.line(item.medianStart.x, item.medianStart.y,
                        item.medianEnd.x, item.medianEnd.y, median);
        }
        writer.text(item.centerX, plot.y + plot.height + 17.0,
                    BoxplotInnermostCategoryLabel(layout, item.category), labels);
    }
    std::size_t depth = 0;
    for (const auto &levels : layout.categoryLevels) depth = std::max(depth, levels.size());
    for (const BoxplotGroupSpan &span : BoxplotGroupSpans(layout)) {
        const double y = plot.y + plot.height + 28.0 +
            16.0 * static_cast<double>(depth - 2 - span.level);
        writer.line(span.startX, y, span.endX, y, outline);
        writer.text(span.centerX, y + 11.0, span.label, labels);
    }
}

std::string SvgPolylinePath(const std::vector<Point> &points)
{
    std::string path;
    for (const Point &point : points) {
        if (!std::isfinite(point.x) || !std::isfinite(point.y)) continue;
        path += (path.empty() ? "M " : " L ") + std::to_string(point.x) + " " +
                std::to_string(point.y);
    }
    return path;
}

void DrawSnapshotTrellis(SvgWriter &writer, const PlotModel &model, const Rect &bounds,
                         const PlotThemeStyleSpec &theme, const std::string &title)
{
    // The screen and vector backends deliberately consume the same portable
    // layout and panel render plans.  Export must therefore preserve the
    // actual adaptive row/column arrangement, free/shared scales and every
    // panel geometry instead of reconstructing an approximate square grid.
    const TrellisScatterplotLayout layout = BuildTrellisScatterplotLayout(model, bounds);
    const std::set<CaseId> selection;
    const auto plans = BuildTrellisPanelRenderPlans(
        model, layout, selection, model.frozenRowColors);
    const TrellisPlotType type = model.trellisSpecificationInitialized
        ? model.trellisSpecification.plotType : TrellisPlotType::Scatter;

    const SvgPaint grid{"none", SvgColor(theme.majorGrid), 0.7,
                        theme.majorGrid.a, {}};
    const SvgPaint axis{"none", SvgColor(theme.axis), 1.0, theme.axis.a, {}};
    const SvgPaint geometry{"none", SvgColor(theme.geomStroke), 1.0,
                            theme.geomStroke.a, {}};
    const SvgTextStyle titleStyle{SvgColor(theme.text),
        "Arial, Helvetica, sans-serif", 15.0, 600, false, "middle"};
    const SvgTextStyle stripStyle{SvgColor(theme.text),
        "Arial, Helvetica, sans-serif", 10.0, 400, false, "middle"};
    const SvgTextStyle tickStyle{SvgColor(theme.mutedText),
        "Arial, Helvetica, sans-serif", 9.0, 400, false, "middle"};
    const SvgTextStyle yTickStyle{SvgColor(theme.mutedText),
        "Arial, Helvetica, sans-serif", 9.0, 400, false, "end"};
    const SvgTextStyle axisLabelStyle{SvgColor(theme.text),
        "Arial, Helvetica, sans-serif", 11.0, 400, false, "middle"};
    const SvgTextStyle emptyStyle{SvgColor(theme.mutedText),
        "Arial, Helvetica, sans-serif", 10.0, 400, false, "middle"};

    writer.text(layout.titleRect.x + layout.titleRect.width / 2.0,
                layout.titleRect.y + 17.0,
                title.empty() ? (model.title.empty() ? "Trellis plot" : model.title) : title,
                titleStyle);

    for (std::size_t panelIndex = 0; panelIndex < layout.panels.size(); ++panelIndex) {
        const TrellisPanelLayout &panel = layout.panels[panelIndex];
        writer.rectangle(panel.plotRect.x, panel.plotRect.y,
                         panel.plotRect.width, panel.plotRect.height,
                         {SvgColor(theme.panel),
                          theme.showPanelBorder ? SvgColor(theme.axis) : "none",
                          theme.showPanelBorder ? 0.8 : 0.0,
                          theme.panel.a, {}});
        if (IsValidRect(panel.headerRect)) {
            writer.rectangle(panel.headerRect.x, panel.headerRect.y,
                             panel.headerRect.width, panel.headerRect.height,
                             {SvgColor(theme.majorGrid), "none", 0.0,
                              theme.majorGrid.a * 0.24, {}});
            writer.text(panel.headerRect.x + panel.headerRect.width / 2.0,
                        panel.headerRect.y + 12.0, panel.label, stripStyle);
        }
        if (!panel.rowStripLabel.empty() && IsValidRect(panel.rowStripRect)) {
            writer.rectangle(panel.rowStripRect.x, panel.rowStripRect.y,
                             panel.rowStripRect.width, panel.rowStripRect.height,
                             {SvgColor(theme.majorGrid), "none", 0.0,
                              theme.majorGrid.a * 0.24, {}});
            writer.text(panel.rowStripRect.x + panel.rowStripRect.width / 2.0,
                        panel.rowStripRect.y + panel.rowStripRect.height / 2.0,
                        panel.rowStripLabel, stripStyle, -90.0);
        }
        for (const TrellisAxisTick &tick : panel.xTicks) {
            writer.line(tick.position, panel.plotRect.y, tick.position,
                        panel.plotRect.y + panel.plotRect.height, grid);
            if (panel.showXTickLabels)
                writer.text(tick.position, panel.plotRect.y + panel.plotRect.height + 14.0,
                            tick.label, tickStyle);
        }
        if (type == TrellisPlotType::Boxplot && panel.showXTickLabels &&
            panelIndex < plans.size()) {
            const BoxplotLayout &boxplotLayout = plans[panelIndex].boxplotLayout;
            std::size_t depth = 0;
            for (const auto &levels : boxplotLayout.categoryLevels)
                depth = std::max(depth, levels.size());
            for (const BoxplotGroupSpan &span : BoxplotGroupSpans(boxplotLayout)) {
                const double y = panel.plotRect.y + panel.plotRect.height + 19.0 +
                    16.0 * static_cast<double>(depth - 2 - span.level);
                writer.line(span.startX, y, span.endX, y, grid);
                writer.text(span.centerX, y + 10.0, span.label, tickStyle);
            }
        }
        for (const TrellisAxisTick &tick : panel.yTicks) {
            writer.line(panel.plotRect.x, tick.position,
                        panel.plotRect.x + panel.plotRect.width, tick.position, grid);
            if (panel.showYTickLabels) {
                if (model.trellisRowStripsOnLeft) {
                    SvgTextStyle rightTickStyle = yTickStyle;
                    rightTickStyle.anchor = "start";
                    writer.text(panel.plotRect.x + panel.plotRect.width + 6.0,
                                tick.position + 3.0, tick.label, rightTickStyle);
                } else {
                    writer.text(panel.plotRect.x - 6.0, tick.position + 3.0,
                                tick.label, yTickStyle);
                }
            }
        }
        writer.line(panel.plotRect.x, panel.plotRect.y + panel.plotRect.height,
                    panel.plotRect.x + panel.plotRect.width,
                    panel.plotRect.y + panel.plotRect.height, axis);
        const double yAxisX = model.trellisRowStripsOnLeft
            ? panel.plotRect.x + panel.plotRect.width : panel.plotRect.x;
        writer.line(yAxisX, panel.plotRect.y, yAxisX,
                    panel.plotRect.y + panel.plotRect.height, axis);
        if (!panel.hasObservations)
            writer.text(panel.plotRect.x + panel.plotRect.width / 2.0,
                        panel.plotRect.y + panel.plotRect.height / 2.0,
                        "No observations", emptyStyle);
        if (panelIndex >= plans.size()) continue;

        const TrellisPanelRenderPlan &plan = plans[panelIndex];
        writer.beginClipRect(panel.plotRect.x, panel.plotRect.y,
                             panel.plotRect.width, panel.plotRect.height);
        if (type == TrellisPlotType::Scatter || type == TrellisPlotType::TimeSeries) {
            if (type == TrellisPlotType::TimeSeries) {
                for (const TrellisTimeSeriesLine &series : plan.timeSeriesLines) {
                    const PlotRGBA color = model.trellisSpecification.groupingVariableId.empty()
                        ? theme.geomStroke : PlotColorForNameOrHex(series.colorKey);
                    writer.path(SvgPolylinePath(series.points),
                        {"none", SvgColor(color), 2.2, 0.96, {}});
                }
            }
            for (const ScatterplotSmoothCurveDrawItem &curve : plan.scatterplot.smoothCurves) {
                if (curve.confidencePolygon.size() < 3) continue;
                const PlotRGBA color = curve.colorName.empty()
                    ? theme.smooth : PlotColorForNameOrHex(curve.colorName);
                writer.path(SvgPolylinePath(curve.confidencePolygon) + " Z",
                    {SvgColor(color), "none", 0.0, curve.confidenceAlpha, {}});
            }
            for (const ScatterplotOverlayDrawItem &overlay : plan.scatterplot.overlayLines) {
                const PlotRGBA color = overlay.useDefaultDarkColor
                    ? theme.geomStroke : PlotColorForNameOrHex(overlay.colorName);
                writer.line(overlay.start.x, overlay.start.y, overlay.end.x, overlay.end.y,
                    {"none", SvgColor(color), overlay.lineWidth, overlay.alpha,
                     overlay.dashed ? std::vector<double>{4.0, 3.0}
                                    : std::vector<double>{}});
            }
            for (const ScatterplotSmoothCurveDrawItem &curve : plan.scatterplot.smoothCurves) {
                writer.path(SvgPolylinePath(curve.points),
                    {"none", SvgColor(PlotColorForNameOrHex(curve.colorName)),
                     curve.lineWidth, curve.alpha,
                     curve.dashed ? std::vector<double>{4.0, 3.0}
                                  : std::vector<double>{}});
            }
            for (const ScatterplotPointDrawItem &point : plan.scatterplot.points) {
                const PlotRGBA explicitColor = point.hasExplicitColor
                    ? PlotColorForNameOrHex(point.colorName) : theme.markFill;
                const PlotRGBA fill = point.hasExplicitColor ? explicitColor : theme.markFill;
                const PlotRGBA stroke = point.hasExplicitColor ? explicitColor : theme.markStroke;
                const double radius = point.radius *
                    ClampPointSizeScale(model.pointSizeScale);
                writer.ellipse(point.point.x, point.point.y, radius, radius,
                    {SvgColor(PlotMarkColor(fill, theme.panel, point.shadeOverlap, point.fillAlpha)),
                     SvgColor(PlotMarkColor(stroke, theme.panel, point.shadeOverlap, point.strokeAlpha)),
                     point.strokeWidth, point.shadeOverlap ? point.fillAlpha : 1.0, {}});
            }
        } else if (type == TrellisPlotType::Histogram) {
            for (const HistogramBarRenderItem &bar : plan.histogram.bars) {
                writer.rectangle(bar.rect.x, bar.rect.y, bar.rect.width, bar.rect.height,
                    {SvgColor(theme.geomFill), "none", 0.0,
                     theme.geomFill.a, {}});
                for (const HistogramBarSegment &segment : bar.colorSegments) {
                    const PlotRGBA color = segment.colorName.empty()
                        ? theme.geomFill
                        : PlotLightColorForNameOrHex(segment.colorName);
                    writer.rectangle(segment.rect.x, segment.rect.y,
                                     segment.rect.width, segment.rect.height,
                                     {SvgColor(color), "none", 0.0, segment.alpha, {}});
                }
                for (const HistogramBarSegment &segment : bar.selectedSegments) {
                    const PlotRGBA color = segment.defaultSelection
                        ? theme.selectedMarkFill
                        : PlotSelectedColorForNameOrHex(segment.colorName);
                    writer.rectangle(segment.rect.x, segment.rect.y,
                                     segment.rect.width, segment.rect.height,
                                     {SvgColor(color), "none", 0.0, segment.alpha, {}});
                }
                writer.rectangle(bar.rect.x, bar.rect.y, bar.rect.width, bar.rect.height,
                    {"none", SvgColor(PlotThemeWhite(0.0)), 1.0, 1.0, {}});
            }
        } else if (type == TrellisPlotType::Boxplot) {
            for (const BoxplotStatsRenderItem &item : plan.boxplotStats) {
                writer.line(item.upperWhiskerStart.x, item.upperWhiskerStart.y,
                            item.upperWhiskerEnd.x, item.upperWhiskerEnd.y, geometry);
                writer.line(item.lowerWhiskerStart.x, item.lowerWhiskerStart.y,
                            item.lowerWhiskerEnd.x, item.lowerWhiskerEnd.y, geometry);
                writer.line(item.upperCapStart.x, item.upperCapStart.y,
                            item.upperCapEnd.x, item.upperCapEnd.y, geometry);
                writer.line(item.lowerCapStart.x, item.lowerCapStart.y,
                            item.lowerCapEnd.x, item.lowerCapEnd.y, geometry);
                writer.rectangle(item.boxRect.x, item.boxRect.y,
                                 item.boxRect.width, item.boxRect.height,
                                 {SvgColor(theme.geomFill), SvgColor(theme.geomStroke), 1.4,
                                  theme.geomFill.a, {}});
                writer.line(item.medianStart.x, item.medianStart.y,
                            item.medianEnd.x, item.medianEnd.y,
                            {"none", SvgColor(theme.accent), 1.5, theme.accent.a, {}});
            }
            for (const BoxplotPointDrawItem &point : plan.boxplotPoints) {
                const auto found = model.frozenRowColors.find(point.caseId);
                const bool explicitColor = found != model.frozenRowColors.end();
                const PlotRGBA fill = explicitColor
                    ? PlotColorForNameOrHex(found->second) : theme.markFill;
                const PlotRGBA stroke = explicitColor ? fill : theme.markStroke;
                const double radius = 2.8 * ClampPointSizeScale(model.pointSizeScale);
                writer.ellipse(point.point.x, point.point.y, radius, radius,
                    {SvgColor(fill), SvgColor(stroke), 0.7,
                     point.dimmed ? 0.24 : 0.65, {}});
            }
        } else if (type == TrellisPlotType::Bar) {
            const std::vector<Rect> bars = BarplotBarRects(plan.barplotLayout);
            for (std::size_t index = 0;
                 index < bars.size() && index < plan.barplotBins.size(); ++index) {
                const Rect &bar = bars[index];
                writer.rectangle(bar.x, bar.y, bar.width, bar.height,
                    {SvgColor(theme.panel), SvgColor(theme.geomStroke), 0.8,
                     theme.panel.a, {}});
                if (index >= plan.barplotSegments.size()) continue;
                for (const BarplotSegmentDrawItem &segment : plan.barplotSegments[index]) {
                    const PlotRGBA color = segment.style.colorKey.empty()
                        ? theme.geomFill : PlotColorForNameOrHex(segment.style.colorKey);
                    writer.rectangle(segment.rect.x, segment.rect.y,
                                     segment.rect.width, segment.rect.height,
                                     {SvgColor(color), SvgColor(theme.geomStroke), 0.8,
                                      segment.style.alpha, {}});
                }
            }
        }
        writer.endClip();

        if (type == TrellisPlotType::TimeSeries) {
            for (const TimeSeriesLegendItem &item : plan.timeSeriesLegend) {
                if (item.seriesIndex >= plan.timeSeriesLines.size()) continue;
                const TrellisTimeSeriesLine &series = plan.timeSeriesLines[item.seriesIndex];
                const PlotRGBA color = PlotColorForNameOrHex(series.colorKey);
                writer.line(item.sampleStart.x, item.sampleStart.y,
                            item.sampleEnd.x, item.sampleEnd.y,
                            {"none", SvgColor(color), 2.0, color.a, {}});
                SvgTextStyle legendStyle{SvgColor(color), "Arial, Helvetica, sans-serif",
                                         9.0, 400, false, "start"};
                writer.text(item.labelAnchor.x, item.labelAnchor.y + 3.0,
                            series.label, legendStyle);
            }
        }
    }
    writer.text(layout.xAxisLabelAnchor.x, layout.xAxisLabelAnchor.y + 12.0,
                model.xLabel, axisLabelStyle);
    writer.text(layout.yAxisLabelAnchor.x, layout.yAxisLabelAnchor.y,
                model.yLabel, axisLabelStyle, -90.0);
}

std::vector<std::string> SplitSvgTableLine(const std::string &line)
{
    std::vector<std::string> cells;
    std::size_t start = 0;
    for (;;) {
        const std::size_t tab = line.find('\t', start);
        cells.push_back(line.substr(start, tab == std::string::npos ? tab : tab - start));
        if (tab == std::string::npos) break;
        start = tab + 1;
    }
    return cells;
}

std::size_t ApproximateUtf8Characters(const std::string &text)
{
    std::size_t count = 0;
    for (unsigned char byte : text) {
        if ((byte & 0xc0u) != 0x80u) ++count;
    }
    return count;
}

void DrawSnapshotColorLegend(SvgWriter &writer,
                             const PlotModel &model,
                             const PlotThemeStyleSpec &theme,
                             double width,
                             double height)
{
    if (!model.colorByLegendVisible || model.colorByVariable.empty() ||
        model.colorByLegendItems.empty()) return;
    if (model.frozenAppearanceCaptured &&
        !PlotColorLegendMatchesLinkedRowColors(model, model.frozenRowColors)) return;
    std::size_t longest = model.colorByVariable.size();
    for (const auto &item : model.colorByLegendItems)
        longest = std::max(longest, ApproximateUtf8Characters(item.first));
    const double legendWidth = std::max(108.0,
        std::min(260.0, 48.0 + 7.2 * static_cast<double>(longest)));
    const double legendHeight = 24.0 + 22.0 *
        static_cast<double>(model.colorByLegendItems.size());
    const double left = std::clamp(model.colorByLegendX * width, 4.0,
        std::max(4.0, width - legendWidth - 4.0));
    const double top = std::clamp(model.colorByLegendY * height, 4.0,
        std::max(4.0, height - legendHeight - 4.0));
    writer.rectangle(left, top, legendWidth, legendHeight,
        {SvgColor(theme.panel), SvgColor(theme.majorGrid), 0.6, 0.96, {}}, 4.0);
    writer.text(left + 6.0, top + 14.0, model.colorByVariable,
        {SvgColor(theme.text), "Arial, Helvetica, sans-serif", 10.0, 700, false, "start"});
    double y = top + 35.0;
    for (const auto &item : model.colorByLegendItems) {
        writer.ellipse(left + 16.0, y, 3.0, 3.0,
            {SvgColor(PlotColorForNameOrHex(item.second)), "none", 0.0, 1.0, {}});
        writer.text(left + 27.0, y + 4.0, item.first,
            {SvgColor(theme.text), "Arial, Helvetica, sans-serif", 10.0, 500, false, "start"});
        y += 22.0;
    }
}

} // namespace

SvgTableDocument BuildSvgPlotSnapshotDocument(const PlotModel &model,
                                              const std::string &themeName,
                                              const std::string &title,
                                              double widthPoints,
                                              double heightPoints)
{
    widthPoints = std::max(320.0, widthPoints);
    heightPoints = std::max(240.0, heightPoints);
    SvgTableDocument document;
    document.dimensions = {widthPoints, heightPoints, 2.0};
    SvgWriter writer(document.dimensions);
    const PlotThemeStyleSpec theme = PlotThemeStyleForName(themeName);
    writer.rectangle(0.0, 0.0, widthPoints, heightPoints,
                     {SvgColor(theme.background), "none", 0.0, theme.background.a, {}});
    if (model.kind == "trellis_scatterplot") {
        DrawSnapshotTrellis(writer, model, {0.0, 0.0, widthPoints, heightPoints}, theme,
                            title.empty() ? model.title : title);
        DrawSnapshotColorLegend(writer, model, theme, widthPoints, heightPoints);
        document.svg = writer.finish();
        return document;
    }
    const bool effectPlot = model.kind == "glm_interaction" ||
        model.kind == "interaction_plot";
    Rect plot = effectPlot
        ? RegressionEffectPlotRect(model, widthPoints, heightPoints)
        : Rect{70.0, 52.0, widthPoints - 100.0, heightPoints - 116.0};
    SvgTextStyle titleStyle{SvgColor(theme.text), "Arial, Helvetica, sans-serif",
                            effectPlot ? 15.0 : 16.0, 600, false,
                            effectPlot ? "middle" : "start"};
    const std::string displayTitle = title.empty() ? model.title : title;
    const auto titleLines = effectPlot && displayTitle == model.title
        ? RegressionEffectTitleLines(model, widthPoints)
        : std::vector<std::string>{displayTitle};
    for (std::size_t index = 0; index < titleLines.size(); ++index)
        writer.text(effectPlot ? plot.x + plot.width / 2.0 : 28.0,
                    (effectPlot ? 26.0 : 28.0) + 18.0 * index,
                    titleLines[index], titleStyle);
    const auto diagnosticStatus = !model.subtitle.empty()
        ? model.subtitle : ImputationDiagnosticDisplayStatus(model);
    if (!diagnosticStatus.empty()) writer.text(
        effectPlot ? plot.x + plot.width / 2.0 : plot.x,
        43.0 + (effectPlot ? 18.0 *
            (std::max<std::size_t>(1, titleLines.size()) - 1) : 0.0),
        diagnosticStatus,
        {SvgColor(theme.mutedText),"Arial, Helvetica, sans-serif",9.5,400,false,
         effectPlot ? "middle" : "start"});
    writer.rectangle(plot.x, plot.y, plot.width, plot.height,
                     {SvgColor(theme.panel), theme.showPanelBorder ? SvgColor(theme.axis) : "none",
                      theme.showPanelBorder ? 0.8 : 0.0, theme.panel.a, {}});
    if (!(model.kind == "boxplot" && BoxplotUsesVariableAxes(model)))
        DrawSnapshotAxes(writer, model, plot, theme);
    if (model.kind == "histogram") DrawSnapshotHistogram(writer, model, plot, theme);
    else if (model.kind == "barplot") DrawSnapshotBars(writer, model, plot, theme);
    else if (model.kind == "boxplot") DrawSnapshotBoxes(writer, model, plot, theme);
    else DrawSnapshotPointCloud(writer, model, plot, theme);
    DrawSnapshotColorLegend(writer, model, theme, widthPoints, heightPoints);
    document.svg = writer.finish();
    return document;
}

VectorTableLayout BuildVectorTableLayout(const std::string &title,
                                         const std::string &tabDelimitedText,
                                         double maximumWidthPoints)
{
    std::vector<std::vector<std::string>> rows;
    std::istringstream input(tabDelimitedText);
    std::string line;
    while (std::getline(input, line)) {
        if (!line.empty() && line.back() == '\r') line.pop_back();
        if (line.empty() && rows.empty()) continue;
        rows.push_back(SplitSvgTableLine(line));
    }
    if (rows.empty()) rows.push_back({"No results"});
    std::size_t columns = 1;
    for (const auto &row : rows) columns = std::max(columns, row.size());
    std::vector<double> widths(columns, 72.0);
    for (const auto &row : rows) {
        for (std::size_t column = 0; column < row.size(); ++column) {
            const double estimated = 18.0 + 6.35 * static_cast<double>(ApproximateUtf8Characters(row[column]));
            widths[column] = std::max(widths[column], std::min(estimated, 280.0));
        }
    }
    const double margin = 24.0;
    const double requestedWidth = margin * 2.0 +
        std::accumulate(widths.begin(), widths.end(), 0.0);
    double width = std::max(240.0, std::min(std::max(240.0, maximumWidthPoints), requestedWidth));
    const double available = width - margin * 2.0;
    const double natural = std::accumulate(widths.begin(), widths.end(), 0.0);
    if (natural > available && natural > 0.0) {
        std::vector<bool> protectedColumn(columns, false);
        if (!rows.empty()) {
            for (std::size_t column = 0; column < rows.front().size(); ++column) {
                std::string header = rows.front()[column];
                std::transform(header.begin(), header.end(), header.begin(), [](unsigned char c) {
                    return static_cast<char>(std::tolower(c));
                });
                protectedColumn[column] = header.find("ci") != std::string::npos ||
                    header.find("confidence") != std::string::npos ||
                    header.find("lower 95") != std::string::npos ||
                    header.find("upper 95") != std::string::npos ||
                    header.find("95%") != std::string::npos;
            }
        }
        double shrinkable = 0.0;
        for (std::size_t column = 0; column < columns; ++column) {
            if (!protectedColumn[column]) shrinkable += std::max(0.0, widths[column] - 54.0);
        }
        const double shortage = natural - available;
        const double fraction = shrinkable > 0.0 ? std::min(1.0, shortage / shrinkable) : 0.0;
        for (std::size_t column = 0; column < columns; ++column) {
            if (!protectedColumn[column]) {
                widths[column] -= fraction * std::max(0.0, widths[column] - 54.0);
            }
        }
        // If the maximum cannot contain the protected values even after other
        // columns reach their floor, expand the vector canvas.  Clipping a
        // confidence interval is more harmful than producing a wider export.
        const double fitted = std::accumulate(widths.begin(), widths.end(), 0.0);
        if (fitted > available) width = margin * 2.0 + fitted;
    }
    const double titleHeight = title.empty() ? 18.0 : 48.0;
    const double rowHeight = 24.0;
    const double height = margin + titleHeight + rowHeight * static_cast<double>(rows.size()) + margin;
    VectorTableLayout layout;
    layout.dimensions = {width, height, 2.0};
    layout.title = title;
    layout.rows = std::move(rows);
    layout.columnWidths = std::move(widths);
    layout.columnStarts.resize(columns, margin);
    for (std::size_t column = 1; column < columns; ++column) {
        layout.columnStarts[column] = layout.columnStarts[column - 1] + layout.columnWidths[column - 1];
    }
    layout.margin = margin;
    layout.titleHeight = titleHeight;
    layout.tableTop = margin + titleHeight;
    layout.rowHeight = rowHeight;
    return layout;
}

SvgTableDocument BuildSvgTableDocument(const std::string &title,
                                       const std::string &tabDelimitedText,
                                       double maximumWidthPoints)
{
    const VectorTableLayout layout = BuildVectorTableLayout(title, tabDelimitedText, maximumWidthPoints);
    const double width = layout.dimensions.widthPoints;
    const double height = layout.dimensions.heightPoints;
    SvgTableDocument document;
    document.dimensions = layout.dimensions;
    SvgWriter writer(document.dimensions);
    writer.rectangle(0, 0, width, height, {"#ffffff", "none", 0.0, 1.0, {}});
    if (!layout.title.empty()) {
        SvgTextStyle titleStyle;
        titleStyle.fontSize = 16.0;
        titleStyle.fontWeight = 600;
        writer.text(layout.margin, layout.margin + 17.0, layout.title, titleStyle);
    }
    const double top = layout.tableTop;
    SvgPaint rule;
    rule.stroke = "#8a8a8a";
    rule.strokeWidth = 0.8;
    writer.line(layout.margin, top, width - layout.margin, top, rule);
    for (std::size_t row = 0; row < layout.rows.size(); ++row) {
        const double y = top + layout.rowHeight * static_cast<double>(row);
        if (row == 0) {
            writer.rectangle(layout.margin, y, width - 2.0 * layout.margin, layout.rowHeight,
                             {"#f2f2f2", "none", 0.0, 1.0, {}});
        }
        for (std::size_t column = 0; column < layout.columnWidths.size(); ++column) {
            const std::string value = column < layout.rows[row].size() ? layout.rows[row][column] : std::string();
            const double cellX = layout.columnStarts[column];
            const double cellWidth = layout.columnWidths[column];
            writer.beginClipRect(cellX + 1.0, y + 1.0, std::max(0.0, cellWidth - 2.0), layout.rowHeight - 2.0);
            SvgTextStyle style;
            style.fontSize = 11.0;
            style.fontWeight = row == 0 ? 600 : 400;
            writer.text(cellX + 6.0, y + 16.0, value, style);
            writer.endClip();
        }
        if (row == 0 || row + 1 == layout.rows.size()) {
            writer.line(layout.margin, y + layout.rowHeight,
                        width - layout.margin, y + layout.rowHeight, rule);
        }
    }
    document.svg = writer.finish();
    return document;
}

} // namespace core
} // namespace rlispstat
