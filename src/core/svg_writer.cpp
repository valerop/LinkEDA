#include "svg_writer.h"

#include "boxplot_model.h"
#include "format_model.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <iomanip>
#include <numeric>
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

void DrawSnapshotAxes(SvgWriter &writer, const PlotModel &model, const Rect &plot,
                      const PlotThemeStyleSpec &theme)
{
    SvgPaint grid{"none", SvgColor(theme.majorGrid), 0.7, theme.majorGrid.a, {}};
    for (int tick = 0; tick <= 5; ++tick) {
        const double fraction = static_cast<double>(tick) / 5.0;
        writer.line(plot.x, plot.y + fraction * plot.height,
                    plot.x + plot.width, plot.y + fraction * plot.height, grid);
        writer.line(plot.x + fraction * plot.width, plot.y,
                    plot.x + fraction * plot.width, plot.y + plot.height, grid);
    }
    SvgPaint axis{"none", SvgColor(theme.axis), 1.0, theme.axis.a, {}};
    writer.line(plot.x, plot.y + plot.height, plot.x + plot.width, plot.y + plot.height, axis);
    writer.line(plot.x, plot.y, plot.x, plot.y + plot.height, axis);
    SvgTextStyle labels;
    labels.fill = SvgColor(theme.text);
    labels.fontSize = 12.0;
    labels.anchor = "middle";
    writer.text(plot.x + plot.width / 2.0, plot.y + plot.height + 42.0,
                model.xLabel, labels);
    writer.text(20.0, plot.y + plot.height / 2.0, model.yLabel, labels, -90.0);
}

void DrawSnapshotPointCloud(SvgWriter &writer, const PlotModel &model, const Rect &plot,
                            const PlotThemeStyleSpec &theme)
{
    const DataViewport viewport = SnapshotViewport(model);
    SvgPaint line{"none", SvgColor(theme.markStroke), 1.2, theme.markStroke.a, {}};
    if (model.kind == "time_series" || model.kind == "line" ||
        !model.interactionPlotLines.empty()) {
        if (!model.interactionPlotLines.empty()) {
            for (const InteractionPlotLine &series : model.interactionPlotLines) {
                std::string path;
                for (const DataPoint &point : series.points) {
                    Point screen = DataToScreen({point.x, point.y}, viewport, plot, true);
                    path += (path.empty() ? "M " : " L ") + std::to_string(screen.x) + " " + std::to_string(screen.y);
                }
                writer.path(path, line);
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
    SvgPaint mark{SvgColor(theme.markFill), SvgColor(theme.markStroke), 0.8,
                  theme.markFill.a, {}};
    SvgTextStyle pointLabel{SvgColor(theme.text), "Arial, Helvetica, sans-serif",
                            9.0, 400, false, "start"};
    for (const DataPoint &point : model.points) {
        if (!std::isfinite(point.x) || !std::isfinite(point.y)) continue;
        Point screen = DataToScreen({point.x, point.y}, viewport, plot, true);
        SvgPaint pointPaint = mark;
        auto color = model.frozenRowColors.find(point.row);
        if (color != model.frozenRowColors.end() && !color->second.empty()) {
            const PlotRGBA explicitColor = PlotColorForNameOrHex(color->second);
            pointPaint.fill = SvgColor(explicitColor);
            pointPaint.stroke = SvgColor(explicitColor);
        }
        writer.ellipse(screen.x, screen.y, 3.0, 3.0, pointPaint);
        if (model.labelDisplayMode == "all") {
            auto label = model.frozenRowLabels.find(point.row);
            if (label != model.frozenRowLabels.end())
                writer.text(screen.x + 5.0, screen.y - 4.0, label->second, pointLabel);
        }
    }
    SvgPaint smooth{"none", SvgColor(theme.smooth), 1.8, theme.smooth.a, {}};
    for (const SmoothCurveData &curve : model.smoothCurves) {
        if (!curve.ok || curve.x.size() < 2 || curve.x.size() != curve.y.size()) continue;
        std::string path;
        for (std::size_t index = 0; index < curve.x.size(); ++index) {
            Point screen = DataToScreen({curve.x[index], curve.y[index]}, viewport, plot, true);
            path += (path.empty() ? "M " : " L ") + std::to_string(screen.x) + " " + std::to_string(screen.y);
        }
        writer.path(path, smooth);
    }
}

void DrawSnapshotHistogram(SvgWriter &writer, const PlotModel &model, const Rect &plot,
                           const PlotThemeStyleSpec &theme)
{
    int maximum = 1;
    for (const HistogramBin &bin : model.histogramBins)
        maximum = std::max(maximum, static_cast<int>(bin.rows.size()));
    const double width = model.histogramBins.empty() ? plot.width :
        plot.width / static_cast<double>(model.histogramBins.size());
    SvgPaint paint{SvgColor(theme.geomFill), SvgColor(theme.geomStroke), 1.0,
                   theme.geomFill.a, {}};
    for (std::size_t index = 0; index < model.histogramBins.size(); ++index) {
        const double height = plot.height * static_cast<double>(model.histogramBins[index].rows.size()) /
            static_cast<double>(maximum);
        writer.rectangle(plot.x + index * width + 1.0, plot.y + plot.height - height,
                         std::max(0.0, width - 2.0), height, paint);
    }
}

void DrawSnapshotBars(SvgWriter &writer, const PlotModel &model, const Rect &plot,
                      const PlotThemeStyleSpec &theme)
{
    int maximum = 1;
    for (const BarplotBin &bin : model.barplotBins) maximum = std::max(maximum, bin.n);
    const double slot = model.barplotBins.empty() ? plot.width :
        plot.width / static_cast<double>(model.barplotBins.size());
    SvgPaint paint{SvgColor(theme.geomFill), SvgColor(theme.geomStroke), 1.0,
                   theme.geomFill.a, {}};
    SvgTextStyle labels{SvgColor(theme.text), "Arial, Helvetica, sans-serif", 10.0, 400, false, "middle"};
    for (std::size_t index = 0; index < model.barplotBins.size(); ++index) {
        const BarplotBin &bin = model.barplotBins[index];
        const double height = plot.height * static_cast<double>(bin.n) / static_cast<double>(maximum);
        writer.rectangle(plot.x + index * slot + slot * 0.12, plot.y + plot.height - height,
                         slot * 0.76, height, paint);
        writer.text(plot.x + (index + 0.5) * slot, plot.y + plot.height + 17.0,
                    bin.category, labels);
    }
}

void DrawSnapshotBoxes(SvgWriter &writer, const PlotModel &model, const Rect &plot,
                       const PlotThemeStyleSpec &theme)
{
    const std::vector<BoxplotStats> stats = BoxplotStatsForModel(model);
    if (stats.empty()) return;
    double ymin = model.ymin, ymax = model.ymax;
    if (!(ymax > ymin)) { ymin = stats.front().lower; ymax = stats.front().upper; }
    for (const BoxplotStats &stat : stats) {
        ymin = std::min(ymin, stat.lower); ymax = std::max(ymax, stat.upper);
    }
    if (!(ymax > ymin)) ymax = ymin + 1.0;
    auto sy = [&](double value) {
        return plot.y + plot.height - (value - ymin) / (ymax - ymin) * plot.height;
    };
    const double slot = plot.width / static_cast<double>(stats.size());
    SvgPaint outline{"none", SvgColor(theme.geomStroke), 1.2, theme.geomStroke.a, {}};
    SvgPaint fill{SvgColor(theme.geomFill), SvgColor(theme.geomStroke), 1.2, theme.geomFill.a, {}};
    SvgTextStyle labels{SvgColor(theme.text), "Arial, Helvetica, sans-serif", 10.0, 400, false, "middle"};
    for (std::size_t index = 0; index < stats.size(); ++index) {
        const BoxplotStats &stat = stats[index];
        const double cx = plot.x + (index + 0.5) * slot;
        writer.line(cx, sy(stat.lower), cx, sy(stat.upper), outline);
        writer.rectangle(cx - slot * 0.28, sy(stat.q3), slot * 0.56,
                         std::max(1.0, sy(stat.q1) - sy(stat.q3)), fill);
        writer.line(cx - slot * 0.28, sy(stat.median), cx + slot * 0.28, sy(stat.median), outline);
        writer.text(cx, plot.y + plot.height + 17.0, stat.category, labels);
    }
}

void DrawSnapshotTrellis(SvgWriter &writer, const PlotModel &model, const Rect &plot,
                         const PlotThemeStyleSpec &theme)
{
    const std::size_t panelCount = std::max<std::size_t>(1, model.trellisPanelLevels.size());
    const std::size_t columns = static_cast<std::size_t>(std::ceil(std::sqrt((double)panelCount)));
    const std::size_t rows = (panelCount + columns - 1) / columns;
    const double gap = 14.0;
    const double panelWidth = (plot.width - gap * (columns - 1)) / columns;
    const double panelHeight = (plot.height - gap * (rows - 1)) / rows;
    const DataViewport commonViewport = SnapshotViewport(model);
    SvgPaint border{"none", SvgColor(theme.axis), 0.8, theme.axis.a, {}};
    SvgPaint mark{SvgColor(theme.markFill), SvgColor(theme.markStroke), 0.7,
                  theme.markFill.a, {}};
    SvgTextStyle strip{SvgColor(theme.text), "Arial, Helvetica, sans-serif", 10.0,
                       600, false, "middle"};
    for (std::size_t panelIndex = 0; panelIndex < panelCount; ++panelIndex) {
        const std::size_t row = panelIndex / columns, column = panelIndex % columns;
        Rect panel{plot.x + column * (panelWidth + gap),
                   plot.y + row * (panelHeight + gap), panelWidth, panelHeight};
        writer.rectangle(panel.x, panel.y, panel.width, panel.height,
                         {SvgColor(theme.panel), SvgColor(theme.axis), 0.8, theme.panel.a, {}});
        const std::string panelId = panelIndex < model.trellisPanelLevels.size()
            ? model.trellisPanelLevels[panelIndex] : std::string();
        const std::string label = panelIndex < model.trellisPanelLabels.size()
            ? model.trellisPanelLabels[panelIndex] : panelId;
        writer.text(panel.x + panel.width / 2.0, panel.y + 13.0, label, strip);
        Rect inner{panel.x + 22.0, panel.y + 22.0,
                   std::max(10.0, panel.width - 30.0), std::max(10.0, panel.height - 32.0)};
        writer.line(inner.x, inner.y + inner.height, inner.x + inner.width,
                    inner.y + inner.height, border);
        writer.line(inner.x, inner.y, inner.x, inner.y + inner.height, border);
        for (std::size_t pointIndex = 0; pointIndex < model.points.size(); ++pointIndex) {
            if (pointIndex >= model.trellisPointPanels.size() ||
                model.trellisPointPanels[pointIndex] != panelId) continue;
            const DataPoint &point = model.points[pointIndex];
            Point screen = DataToScreen({point.x, point.y}, commonViewport, inner, true);
            SvgPaint pointPaint = mark;
            auto color = model.frozenRowColors.find(point.row);
            if (color != model.frozenRowColors.end() && !color->second.empty()) {
                const PlotRGBA explicitColor = PlotColorForNameOrHex(color->second);
                pointPaint.fill = SvgColor(explicitColor);
                pointPaint.stroke = SvgColor(explicitColor);
            }
            writer.ellipse(screen.x, screen.y, 2.5, 2.5, pointPaint);
        }
    }
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
    SvgTextStyle titleStyle{SvgColor(theme.text), "Arial, Helvetica, sans-serif",
                            16.0, 600, false, "start"};
    writer.text(28.0, 28.0, title.empty() ? model.title : title, titleStyle);
    Rect plot{70.0, 52.0, widthPoints - 100.0, heightPoints - 116.0};
    writer.rectangle(plot.x, plot.y, plot.width, plot.height,
                     {SvgColor(theme.panel), theme.showPanelBorder ? SvgColor(theme.axis) : "none",
                      theme.showPanelBorder ? 0.8 : 0.0, theme.panel.a, {}});
    if (model.kind == "trellis_scatterplot") {
        DrawSnapshotTrellis(writer, model, plot, theme);
    } else {
        DrawSnapshotAxes(writer, model, plot, theme);
    }
    if (model.kind == "trellis_scatterplot") {}
    else if (model.kind == "histogram") DrawSnapshotHistogram(writer, model, plot, theme);
    else if (model.kind == "barplot") DrawSnapshotBars(writer, model, plot, theme);
    else if (model.kind == "boxplot") DrawSnapshotBoxes(writer, model, plot, theme);
    else DrawSnapshotPointCloud(writer, model, plot, theme);
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
