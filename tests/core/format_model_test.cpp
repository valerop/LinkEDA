#include "../../src/core/format_model.h"

#include <algorithm>
#include <cassert>
#include <cmath>
#include <limits>
#include <string>
#include <vector>

using rlispstat::core::FormatCorrelation;
using rlispstat::core::FormatDouble;
using rlispstat::core::FormatDoubleOrDash;
using rlispstat::core::FormatModelNumber;
using rlispstat::core::FormatModelNumberOrDash;
using rlispstat::core::FormatFactorLabel;
using rlispstat::core::FormatPValue;
using rlispstat::core::FormatPercent;
using rlispstat::core::FormatPercentOrDash;
using rlispstat::core::PlotDerivedColumnSourceLabel;
using rlispstat::core::PlotCopyFailedStatus;
using rlispstat::core::PlotCopyFailedTitle;
using rlispstat::core::PlotColorForName;
using rlispstat::core::PlotColorForNameOrHex;
using rlispstat::core::PlotExportFailedStatus;
using rlispstat::core::PlotExportFailedTitle;
using rlispstat::core::PlotExportFileExtension;
using rlispstat::core::FormatFixedDecimalsLabel;
using rlispstat::core::FormatAutoLabel;
using rlispstat::core::FormatResetRowColorTooltip;
using rlispstat::core::FormatNameColumnHeader;
using rlispstat::core::FormatTypeColumnHeader;
using rlispstat::core::FormatDecimalsColumnHeader;
using rlispstat::core::FormatRoleColumnHeader;
using rlispstat::core::FormatDescriptionColumnHeader;
using rlispstat::core::FormatDefaultResetColorMenuItemTitle;
using rlispstat::core::PlotExportFilename;
using rlispstat::core::PlotExportPanelTitle;
using rlispstat::core::PlotThemeDisplayName;
using rlispstat::core::PlotThemeStyleForName;
using rlispstat::core::PlotThemeIsValid;
using rlispstat::core::PlotThemeMessageTitle;
using rlispstat::core::PlotThemeNames;
using rlispstat::core::PlotThemeRGB;
using rlispstat::core::PlotThemeUnknownStatus;
using rlispstat::core::PlotThemeWhite;
using rlispstat::core::PlotNoActiveStatus;
using rlispstat::core::UnknownColorStatus;
using rlispstat::core::PlotWindowTitle;
using rlispstat::core::SignificanceStars;
using rlispstat::core::PaletteColors;
using rlispstat::core::FindPaletteColor;
using rlispstat::core::PaletteColorAtIndex;
using rlispstat::core::PaletteColorNameAtIndex;
using rlispstat::core::WindowPlacementState;
using rlispstat::core::WindowPlacementNextFrame;
using rlispstat::core::WindowPlacementReset;

int main()
{
    assert(FormatModelNumber(1.2300) == "1.23");
    assert(FormatModelNumber(-0.0) == "0");
    assert(FormatModelNumber(0.0004) == "4e-4");
    assert(FormatModelNumber(0.0006) == "0.001");
    assert(FormatModelNumber(1234.0) == "1.23e+3");
    assert(FormatModelNumberOrDash(NAN) == "\u2014");
    double nan = std::numeric_limits<double>::quiet_NaN();

    assert(FormatDouble(1.23456, 2) == "1.23");
    assert(FormatDouble(nan, 2).empty());
    assert(FormatDoubleOrDash(nan, 2) == "\u2014");

    assert(FormatPValue(0.0004) == "< .001");
    assert(FormatPValue(0.0324) == ".032");
    assert(FormatPValue(nan) == "\u2014");

    assert(FormatPercent(0.1234, 1) == "12.3%");
    assert(FormatPercent(nan, 1).empty());
    assert(FormatPercentOrDash(nan, 1) == "\u2014");

    assert(FormatCorrelation(0.4567) == ".457");
    assert(FormatCorrelation(-0.4567) == "-.457");
    assert(FormatCorrelation(nan) == "\u2014");

    assert(FormatFactorLabel(1.0) == "1");
    assert(FormatFactorLabel(1.2345678901234) == "1.23456789012");
    assert(FormatFactorLabel(nan) == "NA");

    assert(PlotWindowTitle("Observed vs fitted", "y", "x", "cars", "select", "replace") ==
           "Observed vs fitted [cars]");
    assert(PlotWindowTitle("", "mpg", "wt", "cars", "brush", "toggle") ==
           "mpg vs wt [cars]");
    assert(PlotWindowTitle("", "", "wt", "cars", "select", "replace") ==
           " vs wt [cars]");
    assert(PlotDerivedColumnSourceLabel("Custom plot", "mpg", "wt") == "Custom plot");
    assert(PlotDerivedColumnSourceLabel("", "mpg", "wt") == "mpg_vs_wt");
    assert(PlotDerivedColumnSourceLabel("", "", "wt") == "wt");
    assert(PlotExportFilename("plot-1", "PDF") == "LinkEDA-plot-1.pdf");
    assert(PlotExportFilename("plot-1", "PNG") == "LinkEDA-plot-1.png");
    assert(PlotExportFilename("plot-1", "svg", "app") == "app-plot-1.svg");
    assert(PlotExportFileExtension("PDF") == "pdf");
    assert(PlotExportFileExtension("PNG") == "png");
    assert(PlotExportFileExtension("svg") == "svg");
    assert(PlotExportPanelTitle("PDF") == "Export Plot as PDF");
    assert(PlotExportPanelTitle("png") == "Export Plot as PNG");
    assert(PlotExportPanelTitle("svg") == "Export Plot as SVG");
    assert(PlotExportFailedTitle() == "Export Plot");
    assert(PlotExportFailedStatus() == "The active plot view could not be exported.");
    assert(PlotCopyFailedTitle() == "Copy Plot");
    assert(PlotCopyFailedStatus() == "The active plot view could not be copied.");
    assert(PlotThemeMessageTitle() == "Plot Theme");
    assert(PlotThemeUnknownStatus() == "Unknown plot theme.");

    assert(SignificanceStars(0.0004) == "***");
    assert(SignificanceStars(0.004) == "**");
    assert(SignificanceStars(0.04) == "*");
    assert(SignificanceStars(0.2).empty());
    assert(SignificanceStars(nan).empty());

    std::vector<std::string> themes = PlotThemeNames();
    assert(!themes.empty());
    assert(themes.front() == "classic");
    assert(PlotThemeIsValid("cowplot"));
    assert(PlotThemeIsValid("economist"));
    assert(PlotThemeIsValid("manet"));
    assert(PlotThemeIsValid("vista"));
    assert(PlotThemeIsValid("beige"));
    assert(PlotThemeIsValid("datadesk"));
    assert(PlotThemeIsValid("garish"));
    assert(!PlotThemeIsValid("unknown"));
    assert(PlotThemeDisplayName("theme_modern") == "Modern");
    assert(PlotThemeDisplayName("fivethirtyeight") == "FiveThirtyEight");
    assert(PlotThemeDisplayName("manet") == "MANET");
    assert(PlotThemeDisplayName("vista") == "ViSta");
    assert(PlotThemeDisplayName("beige") == "Beige");
    assert(PlotThemeDisplayName("datadesk") == "DataDesk");
    assert(PlotThemeDisplayName("garish") == "Garish");
    assert(PlotThemeDisplayName("unknown") == "Classic");

    auto rgb = PlotThemeRGB(0.1, 0.2, 0.3, 0.4);
    assert(std::abs(rgb.r - 0.1) < 0.001);
    assert(std::abs(rgb.g - 0.2) < 0.001);
    assert(std::abs(rgb.b - 0.3) < 0.001);
    assert(std::abs(rgb.a - 0.4) < 0.001);

    auto white = PlotThemeWhite(0.7, 0.8);
    assert(std::abs(white.r - 0.7) < 0.001);
    assert(std::abs(white.g - 0.7) < 0.001);
    assert(std::abs(white.b - 0.7) < 0.001);
    assert(std::abs(white.a - 0.8) < 0.001);

    auto classic = PlotThemeStyleForName("classic");
    assert(std::abs(classic.background.r - 0.965) < 0.001);
    assert(std::abs(classic.panel.r - 0.992) < 0.001);
    assert(std::abs(classic.accent.g - 0.447) < 0.001);
    assert(classic.showMinorGrid);
    assert(classic.showPanelBorder);

    auto minimal = PlotThemeStyleForName("minimal");
    assert(std::abs(minimal.background.r - 1.0) < 0.001);
    assert(!minimal.showMinorGrid);
    assert(!minimal.showPanelBorder);

    auto modern = PlotThemeStyleForName("theme_modern");
    assert(std::abs(modern.background.r - 0.070) < 0.001);
    assert(std::abs(modern.text.r - 0.925) < 0.001);

    const std::vector<std::string> historicalThemes{
        "classic", "minimal", "bw", "gray", "cowplot", "ipsum", "theme_tq",
        "theme_modern", "tufte", "economist", "fivethirtyeight"
    };
    assert(std::equal(historicalThemes.begin(), historicalThemes.end(), themes.begin()));
    for (const std::string &name : historicalThemes) {
        assert(!PlotThemeStyleForName(name).coordinatedMarks);
    }

    auto manet = PlotThemeStyleForName("manet");
    assert(manet.coordinatedMarks);
    assert(!manet.showMinorGrid);
    assert(std::abs(manet.background.r - 1.0) < 0.001);
    assert(std::abs(manet.background.b - 0.639) < 0.001);
    assert(std::abs(manet.markFill.r - 0.722) < 0.001);
    assert(std::abs(manet.selectedMarkFill.g - 1.0) < 0.001);
    assert(manet.selectedMarkStrokeWidth > 1.5);

    auto vista = PlotThemeStyleForName("vista");
    assert(vista.coordinatedMarks);
    assert(vista.hollowUnselectedMarks);
    assert(std::abs(vista.markFill.b - 0.773) < 0.001);
    assert(std::abs(vista.selectedMarkFill.r - 0.890) < 0.001);
    assert(std::abs(vista.auxiliary.g - 0.667) < 0.001);

    auto beige = PlotThemeStyleForName("beige");
    assert(beige.coordinatedMarks);
    assert(!beige.showMinorGrid);
    assert(std::abs(beige.background.r - 0.965) < 0.001);
    assert(std::abs(beige.panel.b - 0.965) < 0.001);

    auto datadesk = PlotThemeStyleForName("datadesk");
    assert(datadesk.coordinatedMarks);
    assert(!datadesk.showMinorGrid);
    assert(std::abs(datadesk.panel.r - 1.0) < 0.001);
    assert(std::abs(datadesk.markFill.r - 0.02) < 0.001);
    assert(std::abs(datadesk.selectedMarkFill.r - datadesk.markFill.r) < 0.001);
    assert(datadesk.selectedMarkStroke.r > datadesk.selectedMarkFill.r);
    assert(datadesk.crossUnselectedMarks);
    assert(datadesk.squareSelectedMarks);

    auto garish = PlotThemeStyleForName("garish");
    assert(garish.coordinatedMarks);
    assert(garish.showMinorGrid);
    assert(garish.showPanelBorder);
    assert(std::abs(garish.background.r - 1.0) < 0.001);
    assert(std::abs(garish.background.g - 0.310) < 0.001);
    assert(std::abs(garish.panel.g - 0.953) < 0.001);
    assert(std::abs(garish.markFill.b - 1.0) < 0.001);
    assert(std::abs(garish.selectedMarkFill.r - 1.0) < 0.001);
    assert(garish.selectedMarkFill.g < 0.01);
    assert(garish.selectedMarkStrokeWidth >= 2.0);

    auto unknownTheme = PlotThemeStyleForName("unknown");
    assert(std::abs(unknownTheme.background.r - classic.background.r) < 0.001);
    assert(std::abs(unknownTheme.accent.b - classic.accent.b) < 0.001);

    assert(PlotNoActiveStatus() == "No active plot");
    assert(UnknownColorStatus() == "Unknown color.");
    assert(FormatFixedDecimalsLabel() == "Fixed decimals");
    assert(FormatAutoLabel() == "Auto");
    assert(FormatResetRowColorTooltip() == "Remove explicit row color for the current selection");
    assert(FormatNameColumnHeader() == "Name");
    assert(FormatTypeColumnHeader() == "Type");
    assert(FormatDecimalsColumnHeader() == "Decimals");
    assert(FormatRoleColumnHeader() == "Role");
    assert(FormatDescriptionColumnHeader() == "Description");
    assert(FormatDefaultResetColorMenuItemTitle() == "Default / Reset color");

    // Palette
    auto colors = PaletteColors();
    assert(colors.size() == 8);
    assert(colors[0].name == std::string("orange"));
    assert(colors[6].name == std::string("pink"));
    assert(colors[7].name == std::string("yellow"));
    assert(std::abs(colors[7].baseG - 228.0 / 255.0) < 0.00001);
    assert(std::abs(colors[0].baseR - 0.902) < 0.001);
    assert(!colors[0].selectedTextWhite);
    assert(colors[1].selectedTextWhite);

    auto found = FindPaletteColor("blue");
    assert(found.has_value());
    assert(found->name == std::string("blue"));
    assert(std::abs(found->baseG - 0.447) < 0.001);

    auto namedBlue = PlotColorForName("blue", 0.3);
    assert(std::abs(namedBlue.g - found->baseG) < 0.001);
    assert(std::abs(namedBlue.a - 0.3) < 0.001);

    auto gray = PlotColorForName("gray", 0.4);
    assert(std::abs(gray.r - 0.45) < 0.001);
    assert(std::abs(gray.g - 0.45) < 0.001);
    assert(std::abs(gray.a - 0.4) < 0.001);

    auto unknownColor = PlotColorForName("nonexistent");
    assert(std::abs(unknownColor.r - 0.070) < 0.001);
    assert(std::abs(unknownColor.g - 0.080) < 0.001);
    assert(std::abs(unknownColor.b - 0.085) < 0.001);

    auto hexColor = PlotColorForNameOrHex("#336699", 0.5);
    assert(std::abs(hexColor.r - 0.2) < 0.001);
    assert(std::abs(hexColor.g - 0.4) < 0.001);
    assert(std::abs(hexColor.b - 0.6) < 0.001);
    assert(std::abs(hexColor.a - 0.5) < 0.001);

    auto whiteColor = PlotColorForNameOrHex("WHITE", 0.6);
    assert(std::abs(whiteColor.r - 1.0) < 0.001);
    assert(std::abs(whiteColor.a - 0.6) < 0.001);

    auto notFound = FindPaletteColor("nonexistent");
    assert(!notFound.has_value());

    assert(PaletteColorNameAtIndex(0) == "orange");
    assert(PaletteColorNameAtIndex(8) == "orange");
    assert(PaletteColorNameAtIndex(1) == "blue");

    const auto *palIdx = PaletteColorAtIndex(0);
    assert(palIdx != nullptr);
    assert(palIdx->name == std::string("orange"));
    assert(PaletteColorAtIndex(8) == palIdx);

    // Window placement
    WindowPlacementState ws;
    assert(ws.nextX == 40);
    assert(ws.nextY == 60);
    assert(ws.rowHeight == 0);

    auto f1 = WindowPlacementNextFrame(ws, 300, 200, 1440, 900);
    assert(std::abs(f1.x - 40.0) < 0.1);
    assert(std::abs(f1.y - 640.0) < 0.1);
    assert(std::abs(f1.width - 300.0) < 0.1);
    assert(std::abs(f1.height - 200.0) < 0.1);
    assert(ws.nextX == 360);
    assert(ws.rowHeight == 200);

    auto f2 = WindowPlacementNextFrame(ws, 300, 200, 1440, 900);
    assert(std::abs(f2.x - 360.0) < 0.1);

    WindowPlacementReset(ws);
    assert(ws.nextX == 40);
    assert(ws.nextY == 60);
    assert(ws.rowHeight == 0);

    return 0;
}
