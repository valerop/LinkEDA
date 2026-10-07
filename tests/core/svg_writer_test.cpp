#include "../../src/core/svg_writer.h"

#include <cassert>
#include <string>

static std::size_t CountOccurrences(const std::string &text, const std::string &needle)
{
    std::size_t count = 0;
    std::size_t position = 0;
    while ((position = text.find(needle, position)) != std::string::npos) {
        ++count;
        position += needle.size();
    }
    return count;
}

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
    snapshotPlot.colorByVariable = "transmission";
    snapshotPlot.colorByLegendItems = {{"automatic", "blue"}, {"manual", "orange"}};
    SmoothCurveData curve;
    curve.ok = true; curve.x = {2.0, 3.0, 4.0}; curve.y = {24.0, 20.0, 16.0};
    snapshotPlot.smoothCurves.push_back(curve);
    const SvgTableDocument plot = BuildSvgPlotSnapshotDocument(
        snapshotPlot, "manet", u8"Relación wt — mpg", 720, 520);
    assert(plot.svg.find(u8"Relación wt — mpg") != std::string::npos);
    assert(plot.svg.find("<ellipse") != std::string::npos);
    assert(plot.svg.find("<path") != std::string::npos);
    assert(plot.svg.find("transmission") != std::string::npos);
    assert(plot.svg.find("automatic") != std::string::npos);
    assert(plot.svg.find("manual") != std::string::npos);
    assert(plot.svg.find(
        "font-size=\"10\" font-weight=\"700\" text-anchor=\"start\">transmission</text>") !=
        std::string::npos);
    assert(plot.svg.find(">v</text>") == std::string::npos);
    assert(plot.svg.find("data:image") == std::string::npos);
    snapshotPlot.colorByLegendVisible = false;
    const SvgTableDocument hiddenLegend = BuildSvgPlotSnapshotDocument(
        snapshotPlot, "manet", "Hidden legend", 720, 520);
    assert(hiddenLegend.svg.find("transmission") == std::string::npos);

    PlotModel histogramPlot;
    histogramPlot.kind = "histogram";
    histogramPlot.title = "Histogram of score";
    histogramPlot.xLabel = "score";
    histogramPlot.yLabel = "Count";
    histogramPlot.histogramBins = {
        {0.0, 2.0, {1, 2}}, {2.0, 4.0, {3}}
    };
    const SvgTableDocument histogramTicks = BuildSvgPlotSnapshotDocument(
        histogramPlot, "manet", histogramPlot.title, 720, 520);
    // The exported bars use the same inset zero baseline as the native views.
    assert(histogramTicks.svg.find(
        "<rect x=\"86.5\" y=\"68.16\" width=\"292.5\" height=\"377.74\"") !=
        std::string::npos);
    histogramPlot.histogramShowTickMarks = false;
    histogramPlot.histogramShowTickLabels = false;
    const SvgTableDocument histogramWithoutTicks = BuildSvgPlotSnapshotDocument(
        histogramPlot, "manet", histogramPlot.title, 720, 520);
    assert(CountOccurrences(histogramTicks.svg, "<text") ==
           CountOccurrences(histogramWithoutTicks.svg, "<text") + 10);
    assert(CountOccurrences(histogramTicks.svg, "<line") ==
           CountOccurrences(histogramWithoutTicks.svg, "<line") + 10);

    PlotModel barplotPlot;
    barplotPlot.kind = "barplot";
    barplotPlot.barplotBins = {
        {"A", {1, 2}, 2, 66.0, 2.0, {}},
        {"B", {3}, 1, 34.0, 1.0, {}}
    };
    const SvgTableDocument barplotSnapshot = BuildSvgPlotSnapshotDocument(
        barplotPlot, "manet", "Bar chart", 720, 520);
    assert(barplotSnapshot.svg.find(
        "<rect x=\"120.84\" y=\"68.16\" width=\"223.82\" height=\"377.74\"") !=
        std::string::npos);

    PlotModel interactionPlot;
    interactionPlot.kind = "glm_interaction";
    interactionPlot.title = "Effect: happy:planet";
    interactionPlot.xLabel = "happy";
    interactionPlot.yLabel = "Predicted outcome";
    interactionPlot.regressionConfidenceIntervalsVisible = true;
    interactionPlot.regressionConfidenceLevelVisible = true;
    interactionPlot.xmin = 0.8; interactionPlot.xmax = 2.2;
    interactionPlot.ymin = 48.0; interactionPlot.ymax = 58.0;
    interactionPlot.interactionPlotLines = {
        {"planet = Aurelia", "blue", {{1.0, 50.0, 0}, {2.0, 55.0, 0}}},
        {"planet = Borealis", "orange", {{1.0, 51.5, 0}, {2.0, 57.1, 0}}}
    };
    interactionPlot.interactionPlotLines[0].confidenceLower =
        {{1.0, 49.0, 0}, {2.0, 53.5, 0}};
    interactionPlot.interactionPlotLines[0].confidenceUpper =
        {{1.0, 51.0, 0}, {2.0, 56.5, 0}};
    interactionPlot.interactionLegendPosition = "left";
    const SvgTableDocument interactionLeft = BuildSvgPlotSnapshotDocument(
        interactionPlot, "happy_planet", interactionPlot.title, 760, 520);
    assert(interactionLeft.svg.find("planet = Aurelia") != std::string::npos);
    assert(interactionLeft.svg.find("planet = Borealis") != std::string::npos);
    assert(interactionLeft.svg.find("#0072b2") != std::string::npos);
    assert(interactionLeft.svg.find("#e69f00") != std::string::npos);
    assert(CountOccurrences(interactionLeft.svg, "<path") >= 2);
    assert(interactionLeft.svg.find("<clipPath") != std::string::npos);
    assert(interactionLeft.svg.find("data:image") == std::string::npos);
    assert(interactionLeft.svg.find("95% confidence interval") != std::string::npos);
    PlotModel aggregateWithoutIntervals = interactionPlot;
    aggregateWithoutIntervals.glmDiagnosticKind = "boundary_zero_fit";
    for (auto &series : aggregateWithoutIntervals.interactionPlotLines) {
        series.confidenceLower.clear();
        series.confidenceUpper.clear();
    }
    const auto aggregateSnapshot = BuildSvgPlotSnapshotDocument(
        aggregateWithoutIntervals, "boundary_fit", "Floor / ceiling fit", 760, 520);
    assert(aggregateSnapshot.svg.find("95% confidence interval") == std::string::npos);

    assert(interactionLeft.svg.find("opacity=\"0.16\"") != std::string::npos);
    assert(interactionLeft.svg.find("text-anchor=\"middle\"") != std::string::npos);
    interactionPlot.interactionXTicks = {{1.0, "1"}, {2.0, "2"}};
    const SvgTableDocument categoricalInteraction = BuildSvgPlotSnapshotDocument(
        interactionPlot, "happy_planet_factor", interactionPlot.title, 760, 520);
    assert(categoricalInteraction.svg.find("opacity=\"0.16\"") == std::string::npos);
    assert(categoricalInteraction.svg.find(">1</text>") != std::string::npos);
    assert(categoricalInteraction.svg.find(">2</text>") != std::string::npos);
    assert(CountOccurrences(categoricalInteraction.svg, "<line") >
           CountOccurrences(interactionLeft.svg, "<line"));
    interactionPlot.interactionXTicks.clear();
    interactionPlot.regressionConfidenceIntervalsVisible = false;
    interactionPlot.regressionConfidenceLevelVisible = false;
    const SvgTableDocument interactionWithoutConfidence = BuildSvgPlotSnapshotDocument(
        interactionPlot, "happy_planet", interactionPlot.title, 760, 520);
    assert(interactionWithoutConfidence.svg.find("95% confidence interval") == std::string::npos);
    interactionPlot.regressionConfidenceIntervalsVisible = true;
    interactionPlot.regressionConfidenceLevelVisible = true;
    interactionPlot.interactionLegendPosition = "right";
    const SvgTableDocument interactionRight = BuildSvgPlotSnapshotDocument(
        interactionPlot, "happy_planet", interactionPlot.title, 760, 520);
    assert(interactionRight.svg.find("planet = Aurelia") != std::string::npos);
    assert(interactionLeft.svg != interactionRight.svg);
    assert(SetInteractionLegendCustomPosition(interactionPlot, 0.22, 0.31));
    const SvgTableDocument interactionCustom = BuildSvgPlotSnapshotDocument(
        interactionPlot, "happy_planet", interactionPlot.title, 760, 520);
    assert(interactionCustom.svg != interactionRight.svg);
    interactionPlot.subtitle = "Pooled across 20 imputations";
    interactionPlot.interactionLegendTitle = "A long grouping-variable label";
    interactionPlot.interactionLegendTitleVisible = true;
    const auto originalEstimates = interactionPlot.interactionPlotLines;
    for (const std::string &position : {"outside_right", "outside_left",
                                         "outside_top", "outside_bottom"}) {
        assert(SetInteractionLegendPosition(interactionPlot, position));
        const auto exported = BuildSvgPlotSnapshotDocument(
            interactionPlot, "publication", interactionPlot.title, 760, 520);
        assert(exported.svg.find("A long grouping-variable label") != std::string::npos);
        assert(exported.svg.find("Pooled across 20 imputations") != std::string::npos);
        assert(exported.svg.find("planet = Aurelia") != std::string::npos);
        const auto narrow = BuildSvgPlotSnapshotDocument(
            interactionPlot, "publication", interactionPlot.title, 380, 300);
        assert(narrow.svg.find("A long grouping-variable label") != std::string::npos);
    }
    assert(interactionPlot.interactionPlotLines.size() == originalEstimates.size());
    assert(interactionPlot.interactionPlotLines[0].points[0].y ==
           originalEstimates[0].points[0].y);
    interactionPlot.interactionLegendTitleVisible = false;
    const auto hiddenLegendTitle = BuildSvgPlotSnapshotDocument(
        interactionPlot, "publication", interactionPlot.title, 760, 520);
    assert(hiddenLegendTitle.svg.find("A long grouping-variable label") == std::string::npos);
    interactionPlot.interactionLegendTitleVisible = true;

    PlotModel binaryDifference = interactionPlot;
    binaryDifference.title = "Male − Female difference in P(Treatment), by country";
    binaryDifference.xLabel = "gender contrast";
    binaryDifference.yLabel = "Probability difference for Treatment (percentage points)";
    binaryDifference.regressionBinaryProbability = true;
    binaryDifference.regressionEffectQuantity = "probability_difference";
    binaryDifference.regressionConfidenceIntervalsVisible = false;
    binaryDifference.xmin = 0.5; binaryDifference.xmax = 1.5;
    binaryDifference.ymin = -10.0; binaryDifference.ymax = 6.0;
    binaryDifference.interactionPlotLines = {
        {"Finland", "orange", {{1.0, 4.0, 0}}},
        {"Greece", "blue", {{1.0, -6.0, 0}}},
        {"Italy", "green", {{1.0, -9.0, 0}}}
    };
    const SvgTableDocument binaryDifferenceDocument = BuildSvgPlotSnapshotDocument(
        binaryDifference, "happy_planet", binaryDifference.title, 760, 520);
    assert(binaryDifferenceDocument.svg.find("stroke-dasharray=\"5 4\"") != std::string::npos);
    // Three plotted estimates plus three point-shaped legend keys.
    assert(CountOccurrences(binaryDifferenceDocument.svg, "<ellipse") >= 6);
    assert(binaryDifferenceDocument.svg.find("gender contrast") != std::string::npos);

    PlotModel trellis;
    trellis.kind = "trellis_scatterplot";
    trellis.title = "Trellis export";
    trellis.xLabel = "wt";
    trellis.yLabel = "mpg";
    trellis.trellisConditionVariable = "cyl";
    trellis.trellisPanelLevels = {"4", "6", "8"};
    trellis.trellisPanelLabels = {"cyl = 4", "cyl = 6", "cyl = 8"};
    trellis.points = {
        {1.84, 33.9, 1}, {2.32, 22.8, 2},
        {2.62, 21.0, 3}, {3.46, 18.1, 4},
        {3.78, 15.2, 5}, {5.42, 10.4, 6}
    };
    trellis.trellisPointPanels = {"4", "4", "6", "6", "8", "8"};
    trellis.overlays.push_back(Overlay{1, "lm", "all", true});
    trellis.smoothCurves.push_back(
        SmoothCurveData{SmoothCurveScope::Overall, ".", {}, {}, false, "needed"});
    trellis.trellisPanelSmoothCurves["4"] = {
        SmoothCurveData{SmoothCurveScope::Overall, ".",
                        {1.84, 2.08, 2.32}, {33.9, 27.8, 22.8}, true, ""}
    };
    trellis.frozenRowColors[2] = "blue";
    const SvgTableDocument trellisDocument = BuildSvgPlotSnapshotDocument(
        trellis, "manet", "Trellis export", 980, 500);
    assert(trellisDocument.svg.find("width=\"980pt\" height=\"500pt\"") !=
           std::string::npos);
    assert(trellisDocument.svg.find("Trellis export") != std::string::npos);
    assert(trellisDocument.svg.find("cyl = 4") != std::string::npos);
    assert(trellisDocument.svg.find("cyl = 6") != std::string::npos);
    assert(trellisDocument.svg.find("cyl = 8") != std::string::npos);
    assert(trellisDocument.svg.find(">wt<") != std::string::npos);
    assert(trellisDocument.svg.find(">mpg<") != std::string::npos);
    assert(CountOccurrences(trellisDocument.svg, "<clipPath") == 3);
    assert(CountOccurrences(trellisDocument.svg, "<ellipse") == 6);
    assert(trellisDocument.svg.find("<path") != std::string::npos);
    assert(trellisDocument.svg.find("data:image") == std::string::npos);

    PlotModel ungroupedBoxplot;
    ungroupedBoxplot.kind = "boxplot";
    ungroupedBoxplot.title = "wt";
    ungroupedBoxplot.yLabel = "wt";
    ungroupedBoxplot.boxplotVariables = {"wt"};
    ungroupedBoxplot.boxplotCategories = {"wt"};
    ungroupedBoxplot.boxplotDefinedCategories = {"wt"};
    ungroupedBoxplot.boxplotShowPoints = true;
    ungroupedBoxplot.boxplotShowBox = true;
    ungroupedBoxplot.boxplotShowWhiskers = true;
    ungroupedBoxplot.boxplotPoints = {
        {1.8, "wt", 1}, {2.1, "wt", 2}, {2.8, "wt", 3},
        {3.2, "wt", 4}, {3.4, "wt", 5}, {3.8, "wt", 6}, {5.2, "wt", 7}
    };
    const SvgTableDocument ungroupedBoxplotDocument = BuildSvgPlotSnapshotDocument(
        ungroupedBoxplot, "manet", "wt", 720, 520);
    // Background, panel, clipping rectangle and the actual Tukey box.
    assert(CountOccurrences(ungroupedBoxplotDocument.svg, "<rect") >= 4);
    // Whiskers/caps, median and axes must accompany the point layer.
    assert(CountOccurrences(ungroupedBoxplotDocument.svg, "<line") >= 9);
    assert(CountOccurrences(ungroupedBoxplotDocument.svg, "<ellipse") == 7);

    PlotModel parallelCoordinates;
    parallelCoordinates.kind = "boxplot";
    parallelCoordinates.title = "Parallel Coordinates";
    parallelCoordinates.yLabel = "Standardized value";
    parallelCoordinates.boxplotVariables = {"mpg", "cyl", "disp"};
    parallelCoordinates.boxplotCategories = parallelCoordinates.boxplotVariables;
    parallelCoordinates.boxplotDefinedCategories = parallelCoordinates.boxplotVariables;
    parallelCoordinates.boxplotGroupOrder = "defined";
    parallelCoordinates.boxplotShowPoints = true;
    parallelCoordinates.boxplotShowBox = false;
    parallelCoordinates.boxplotShowWhiskers = false;
    parallelCoordinates.boxplotConnectRows = true;
    parallelCoordinates.boxplotConnectionLineWidth = 1.25;
    parallelCoordinates.boxplotPoints = {
        { 1.20, "mpg", 1}, {-0.10, "cyl", 1}, {-0.60, "disp", 1},
        {-0.30, "mpg", 2}, { 0.75, "cyl", 2}, { 0.90, "disp", 2},
        {-1.00, "mpg", 3}, {-0.65, "cyl", 3}, { 1.45, "disp", 3}
    };
    parallelCoordinates.frozenRowColors[2] = "blue";
    const SvgTableDocument parallelDocument = BuildSvgPlotSnapshotDocument(
        parallelCoordinates, "manet", "Parallel Coordinates", 760, 520);
    assert(parallelDocument.svg.find("Parallel Coordinates") != std::string::npos);
    assert(parallelDocument.svg.find("Standardized value") != std::string::npos);
    assert(parallelDocument.svg.find(">mpg<") != std::string::npos);
    assert(parallelDocument.svg.find(">cyl<") != std::string::npos);
    assert(parallelDocument.svg.find(">disp<") != std::string::npos);
    assert(CountOccurrences(parallelDocument.svg, "<path") == 3);
    assert(CountOccurrences(parallelDocument.svg, "<ellipse") == 9);
    // Only the document background, panel, and clipping rectangle are present:
    // disabled boxplot rectangles must not leak into a parallel-coordinates export.
    assert(CountOccurrences(parallelDocument.svg, "<rect") == 3);
    return 0;
}
