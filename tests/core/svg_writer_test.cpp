#include "../../src/core/svg_writer.h"

#include <cassert>
#include <string>

int main()
{
    using namespace rlispstat::core;
    SvgWriter svg(ExportDimensions{640.0, 480.0, 2.0});
    assert(svg.valid());
    SvgPaint linePaint{"none", "#123456", 2.0, 0.75, {4.0, 2.0}};
    SvgPaint fillPaint{"#ffeeaa", "#111111", 1.0, 1.0, {}};
    svg.rectangle(10, 20, 100, 50, fillPaint, 3);
    svg.line(0, 0, 30, 40, linePaint);
    svg.ellipse(50, 60, 4, 5, fillPaint);
    svg.path("M 10 10 L 20 20 Z", linePaint);
    const std::string clip = svg.beginClipRect(5, 5, 200, 100);
    assert(clip == "rls-clip-1");
    SvgTextStyle textStyle;
    textStyle.fontWeight = 600;
    textStyle.italic = true;
    textStyle.anchor = "middle";
    svg.text(100, 80, "A & B < C > D \u2212 \u03b2 long label", textStyle, -15);
    svg.endClip();
    const std::string document = svg.finish();
    assert(document.find("<svg xmlns=\"http://www.w3.org/2000/svg\"") != std::string::npos);
    assert(document.find("width=\"640pt\" height=\"480pt\"") != std::string::npos);
    assert(document.find("viewBox=\"0 0 640 480\"") != std::string::npos);
    assert(document.find("A &amp; B &lt; C &gt; D") != std::string::npos);
    assert(document.find("\u2212 \u03b2") != std::string::npos);
    assert(document.find("stroke-dasharray=\"4 2\"") != std::string::npos);
    assert(document.find("opacity=\"0.75\"") != std::string::npos);
    assert(document.find("clip-path=\"url(#rls-clip-1)\"") != std::string::npos);
    assert(document.find("font-family=\"Arial, Helvetica, sans-serif\"") != std::string::npos);
    assert(document.find("data:image/png") == std::string::npos);
    assert(document == svg.finish());
    assert(!SvgWriter(ExportDimensions{0, 10, 2}).valid());
    assert(SvgWriter::EscapeAttribute("a\"b'c") == "a&quot;b&apos;c");

    const SvgTableDocument table = BuildSvgTableDocument(
        "Model & results − β",
        "Variable\tb\tp\nmpg\t0.041\t.723\ncyl\t—\t.002");
    const VectorTableLayout tableLayout = BuildVectorTableLayout(
        "Model & results − β",
        "Variable\tb\tp\nmpg\t0.041\t.723\ncyl\t—\t.002");
    assert(tableLayout.dimensions.widthPoints == table.dimensions.widthPoints);
    assert(tableLayout.dimensions.heightPoints == table.dimensions.heightPoints);
    assert(tableLayout.rows.size() == 3);
    assert(tableLayout.columnWidths.size() == 3);
    assert(tableLayout.columnStarts[1] == tableLayout.columnStarts[0] + tableLayout.columnWidths[0]);
    assert(table.dimensions.widthPoints > 0.0);
    assert(table.dimensions.heightPoints > 0.0);
    assert(table.svg.find("Model &amp; results") != std::string::npos);
    assert(table.svg.find("font-weight=\"600\"") != std::string::npos);
    assert(table.svg.find("clipPath") != std::string::npos);
    assert(table.svg.find("data:image/png") == std::string::npos);

    const VectorTableLayout narrowIntervalTable = BuildVectorTableLayout(
        "Intervals", "Term\t95% CI\tp\nwt\t[-123456.789, 987654.321]\t.002", 240.0);
    assert(narrowIntervalTable.columnWidths.size() == 3);
    assert(narrowIntervalTable.columnWidths[1] > 118.0);
    assert(narrowIntervalTable.dimensions.widthPoints >=
        2.0 * narrowIntervalTable.margin + narrowIntervalTable.columnWidths[0] +
        narrowIntervalTable.columnWidths[1] + narrowIntervalTable.columnWidths[2]);

    PlotModel snapshotPlot;
    snapshotPlot.kind = "scatter";
    snapshotPlot.xLabel = "wt";
    snapshotPlot.yLabel = "mpg";
    snapshotPlot.xmin = 1.0; snapshotPlot.xmax = 5.0;
    snapshotPlot.ymin = 10.0; snapshotPlot.ymax = 35.0;
    snapshotPlot.points = {{2.62, 21.0, 1}, {3.44, 18.7, 2}};
    SmoothCurveData curve;
    curve.ok = true; curve.x = {2.0, 3.0, 4.0}; curve.y = {24.0, 20.0, 16.0};
    snapshotPlot.smoothCurves.push_back(curve);
    const SvgTableDocument plot = BuildSvgPlotSnapshotDocument(
        snapshotPlot, "manet", u8"Relación wt — mpg", 720, 520);
    assert(plot.svg.find(u8"Relación wt — mpg") != std::string::npos);
    assert(plot.svg.find("<ellipse") != std::string::npos);
    assert(plot.svg.find("<path") != std::string::npos);
    assert(plot.svg.find("data:image") == std::string::npos);
    return 0;
}
