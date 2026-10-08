#include "format_model.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdio>
#include <iomanip>
#include <sstream>
#include <optional>

namespace rlispstat {
namespace core {

std::string FormatDouble(double value, int digits)
{
    if (!std::isfinite(value)) {
        return "";
    }
    std::ostringstream out;
    out << std::fixed << std::setprecision(digits) << value;
    return out.str();
}

std::string FormatDoubleOrDash(double value, int digits)
{
    std::string formatted = FormatDouble(value, digits);
    return formatted.empty() ? "\u2014" : formatted;
}

namespace {

std::string TrimFixedZeroes(std::string value)
{
    const std::size_t decimal = value.find('.');
    if (decimal == std::string::npos) return value;
    while (!value.empty() && value.back() == '0') value.pop_back();
    if (!value.empty() && value.back() == '.') value.pop_back();
    return value == "-0" ? "0" : value;
}

std::string TrimScientificZeroes(std::string value)
{
    const std::size_t exponent = value.find('e');
    if (exponent == std::string::npos) return value;
    std::string mantissa = TrimFixedZeroes(value.substr(0, exponent));
    std::string exponentText = value.substr(exponent + 1);
    char sign = '+';
    std::size_t cursor = 0;
    if (!exponentText.empty() && (exponentText[0] == '+' || exponentText[0] == '-')) {
        sign = exponentText[0];
        cursor = 1;
    }
    while (cursor + 1 < exponentText.size() && exponentText[cursor] == '0') ++cursor;
    return mantissa + "e" + (sign == '-' ? "-" : "+") + exponentText.substr(cursor);
}

} // namespace

std::string FormatModelNumber(double value)
{
    if (!std::isfinite(value)) return "";
    const double magnitude = std::fabs(value);
    const bool roundedFixedWouldBeZero = magnitude > 0.0 && magnitude < 0.0005;
    std::ostringstream out;
    if (magnitude >= 1000.0 || roundedFixedWouldBeZero) {
        out << std::scientific << std::setprecision(2) << value;
        return TrimScientificZeroes(out.str());
    }
    out << std::fixed << std::setprecision(3) << value;
    return TrimFixedZeroes(out.str());
}

std::string FormatModelNumberOrDash(double value)
{
    const std::string formatted = FormatModelNumber(value);
    return formatted.empty() ? "\u2014" : formatted;
}

std::string FormatPValue(double p)
{
    if (!std::isfinite(p)) {
        return "\u2014";
    }
    if (p < 0.001) {
        return "< .001";
    }
    std::ostringstream out;
    out << std::fixed << std::setprecision(3) << p;
    std::string s = out.str();
    if (!s.empty() && s[0] == '0') {
        s.erase(0, 1);
    }
    return s;
}

std::string FormatPercent(double p, int digits)
{
    if (!std::isfinite(p)) {
        return "";
    }
    std::ostringstream out;
    out << std::fixed << std::setprecision(digits) << (100.0 * p) << "%";
    return out.str();
}

std::string FormatPercentOrDash(double p, int digits)
{
    std::string formatted = FormatPercent(p, digits);
    return formatted.empty() ? "\u2014" : formatted;
}

std::string FormatCorrelation(double r)
{
    if (!std::isfinite(r)) {
        return "\u2014";
    }
    std::ostringstream out;
    out << std::fixed << std::setprecision(3) << r;
    std::string s = out.str();
    if (s.rfind("0", 0) == 0) {
        s.erase(0, 1);
    } else if (s.rfind("-0", 0) == 0) {
        s.erase(1, 1);
    }
    return s;
}

std::string FormatFactorLabel(double value)
{
    if (!std::isfinite(value)) {
        return "NA";
    }
    std::ostringstream out;
    out << std::setprecision(12) << value;
    return out.str();
}

std::string PlotWindowTitle(const std::string &title,
                             const std::string &yLabel,
                             const std::string &xLabel,
                             const std::string &group,
                             const std::string &,
                             const std::string &)
{
    std::ostringstream out;
    std::string displayTitle = title.empty() ? (yLabel + " vs " + xLabel) : title;
    out << displayTitle << " [" << group << "]";
    return out.str();
}

std::string PlotDerivedColumnSourceLabel(const std::string &title,
                                         const std::string &yLabel,
                                         const std::string &xLabel)
{
    if (!title.empty()) {
        return title;
    }
    if (yLabel.empty()) {
        return xLabel;
    }
    return yLabel + "_vs_" + xLabel;
}

std::string PlotExportFilename(const std::string &plotId,
                               const std::string &format,
                               const std::string &prefix)
{
    return prefix + "-" + plotId + "." + PlotExportFileExtension(format);
}

std::string PlotExportFileExtension(const std::string &format)
{
    std::string normalized = format;
    std::transform(normalized.begin(), normalized.end(), normalized.begin(),
        [](unsigned char ch) { return static_cast<char>(std::toupper(ch)); });
    if (normalized == "PDF") return "pdf";
    if (normalized == "SVG") return "svg";
    return "png";
}

std::string PlotExportPanelTitle(const std::string &format)
{
    std::string normalized = format;
    std::transform(normalized.begin(), normalized.end(), normalized.begin(),
        [](unsigned char ch) { return static_cast<char>(std::toupper(ch)); });
    if (normalized == "PDF") return "Export Plot as PDF";
    if (normalized == "SVG") return "Export Plot as SVG";
    return "Export Plot as PNG";
}

std::string PlotExportFailedTitle()
{
    return "Export Plot";
}

std::string PlotExportFailedStatus()
{
    return "The active plot view could not be exported.";
}

std::string PlotCopyFailedTitle()
{
    return "Copy Plot";
}

std::string PlotCopyFailedStatus()
{
    return "The active plot view could not be copied.";
}

std::string PlotThemeMessageTitle()
{
    return "Plot Theme";
}

std::string PlotThemeUnknownStatus()
{
    return "Unknown plot theme.";
}

std::string PlotNoActiveStatus()
{
    return "No active plot";
}

std::string UnknownColorStatus()
{
    return "Unknown color.";
}

std::string SignificanceStars(double p)
{
    if (!std::isfinite(p)) {
        return "";
    }
    if (p < 0.001) {
        return "***";
    }
    if (p < 0.01) {
        return "**";
    }
    if (p < 0.05) {
        return "*";
    }
    return "";
}

std::vector<std::string> PlotThemeNames()
{
    return {"publication", "classic", "minimal", "bw", "gray", "cowplot", "ipsum",
            "theme_tq", "theme_modern", "tufte", "economist", "fivethirtyeight",
            "manet", "vista", "beige", "datadesk", "garish"};
}

bool PlotThemeIsValid(const std::string &theme)
{
    std::vector<std::string> names = PlotThemeNames();
    return std::find(names.begin(), names.end(), theme) != names.end();
}

std::string PlotThemeDisplayName(const std::string &theme)
{
    if (theme == "minimal") return "Minimal";
    if (theme == "bw") return "Black and white";
    if (theme == "gray") return "Gray";
    if (theme == "cowplot") return "Cowplot";
    if (theme == "ipsum") return "Ipsum";
    if (theme == "theme_tq") return "theme_tq";
    if (theme == "theme_modern") return "Modern";
    if (theme == "tufte") return "Tufte";
    if (theme == "economist") return "Economist";
    if (theme == "fivethirtyeight") return "FiveThirtyEight";
    if (theme == "manet") return "MANET";
    if (theme == "vista") return "ViSta";
    if (theme == "beige") return "Beige";
    if (theme == "datadesk") return "DataDesk";
    if (theme == "garish") return "Garish";
    if (theme == "publication") return "Default";
    return "Classic";
}

PlotRGBA PlotThemeRGB(double r, double g, double b, double a)
{
    return PlotRGBA{r, g, b, a};
}

PlotRGBA PlotThemeWhite(double w, double a)
{
    return PlotRGBA{w, w, w, a};
}

PlotRGBA PlotMarkColor(PlotRGBA color, PlotRGBA panel, bool shadeOverlap, double alphaMultiplier)
{
    const double alpha = std::clamp(color.a * alphaMultiplier, 0.0, 1.0);
    if (shadeOverlap) return {color.r, color.g, color.b, alpha};
    return {alpha * color.r + (1.0 - alpha) * panel.r,
            alpha * color.g + (1.0 - alpha) * panel.g,
            alpha * color.b + (1.0 - alpha) * panel.b, 1.0};
}

PlotThemeStyleSpec PlotThemeStyleForName(const std::string &theme)
{
    if (theme == "publication") {
        PlotThemeStyleSpec style{
            PlotThemeWhite(1.0), PlotThemeWhite(1.0),
            PlotThemeWhite(0.975), PlotThemeWhite(0.930),
            PlotThemeWhite(0.20), PlotThemeWhite(0.10), PlotThemeWhite(0.38),
            PlotThemeWhite(0.90, 0.82), PlotThemeWhite(0.20),
            PlotThemeRGB(0.000, 0.447, 0.698), PlotThemeRGB(0.902, 0.624, 0.000),
            false, true
        };
        style.showAxisTickMarks = true;
        return style;
    }
    if (theme == "manet") {
        PlotThemeStyleSpec style{
            PlotThemeRGB(1.000, 0.988, 0.639), PlotThemeRGB(1.000, 0.988, 0.639),
            PlotThemeRGB(0.839, 0.827, 0.541, 0.28), PlotThemeRGB(0.839, 0.827, 0.541, 0.55),
            PlotThemeRGB(0.067, 0.067, 0.067), PlotThemeRGB(0.067, 0.067, 0.067), PlotThemeRGB(0.200, 0.200, 0.200),
            PlotThemeRGB(0.722, 0.722, 0.722, 0.90), PlotThemeRGB(0.067, 0.067, 0.067),
            PlotThemeRGB(0.400, 1.000, 0.000), PlotThemeRGB(0.200, 0.200, 0.200),
            false, true
        };
        style.markFill = PlotThemeRGB(0.722, 0.722, 0.722);
        style.markStroke = PlotThemeRGB(0.067, 0.067, 0.067);
        style.selectedMarkFill = PlotThemeRGB(0.400, 1.000, 0.000);
        style.selectedMarkStroke = PlotThemeRGB(0.067, 0.067, 0.067);
        style.auxiliary = PlotThemeRGB(0.200, 0.200, 0.200);
        style.reference = PlotThemeRGB(0.200, 0.200, 0.200);
        style.smooth = PlotThemeRGB(0.133, 0.133, 0.133);
        style.coordinatedMarks = true;
        style.selectedMarkScale = 1.22;
        style.selectedMarkStrokeWidth = 1.9;
        return style;
    }
    if (theme == "vista") {
        PlotThemeStyleSpec style{
            PlotThemeWhite(1.0), PlotThemeWhite(1.0),
            PlotThemeRGB(0.898, 0.898, 0.898, 0.55), PlotThemeRGB(0.898, 0.898, 0.898),
            PlotThemeRGB(0.067, 0.067, 0.067), PlotThemeRGB(0.067, 0.067, 0.067), PlotThemeRGB(0.333, 0.333, 0.333),
            PlotThemeRGB(0.663, 0.706, 0.910, 0.82), PlotThemeRGB(0.204, 0.294, 0.773),
            PlotThemeRGB(0.890, 0.149, 0.180), PlotThemeRGB(0.118, 0.667, 0.294),
            true, true
        };
        style.markFill = PlotThemeRGB(0.204, 0.294, 0.773);
        style.markStroke = PlotThemeRGB(0.204, 0.294, 0.773);
        style.selectedMarkFill = PlotThemeRGB(0.890, 0.149, 0.180);
        style.selectedMarkStroke = PlotThemeRGB(0.067, 0.067, 0.067);
        style.auxiliary = PlotThemeRGB(0.118, 0.667, 0.294);
        style.reference = PlotThemeRGB(0.333, 0.333, 0.333);
        style.smooth = PlotThemeRGB(0.118, 0.667, 0.294);
        style.coordinatedMarks = true;
        style.hollowUnselectedMarks = true;
        style.selectedMarkScale = 1.28;
        style.selectedMarkStrokeWidth = 1.9;
        return style;
    }
    if (theme == "beige") {
        PlotThemeStyleSpec style{
            PlotThemeRGB(0.965, 0.957, 0.918), PlotThemeRGB(1.000, 0.996, 0.965),
            PlotThemeRGB(0.780, 0.773, 0.733, 0.28), PlotThemeRGB(0.640, 0.635, 0.600, 0.62),
            PlotThemeRGB(0.090, 0.090, 0.082), PlotThemeRGB(0.035, 0.035, 0.031), PlotThemeRGB(0.290, 0.286, 0.267),
            PlotThemeRGB(0.870, 0.862, 0.815, 0.88), PlotThemeRGB(0.035, 0.035, 0.031),
            PlotThemeRGB(0.035, 0.035, 0.031), PlotThemeRGB(0.400, 0.392, 0.360),
            false, true
        };
        style.markFill = PlotThemeRGB(0.035, 0.035, 0.031);
        style.markStroke = PlotThemeRGB(0.035, 0.035, 0.031);
        style.selectedMarkFill = PlotThemeRGB(0.035, 0.035, 0.031);
        style.selectedMarkStroke = PlotThemeRGB(0.520, 0.510, 0.475);
        style.auxiliary = PlotThemeRGB(0.180, 0.310, 0.430);
        style.reference = PlotThemeRGB(0.400, 0.392, 0.360);
        style.smooth = PlotThemeRGB(0.620, 0.145, 0.105);
        style.coordinatedMarks = true;
        style.selectedMarkScale = 1.28;
        style.selectedMarkStrokeWidth = 2.0;
        return style;
    }
    if (theme == "datadesk") {
        PlotThemeStyleSpec style{
            PlotThemeWhite(1.0), PlotThemeWhite(1.0),
            PlotThemeWhite(1.0, 0.0), PlotThemeWhite(1.0, 0.0),
            PlotThemeWhite(0.05), PlotThemeWhite(0.02), PlotThemeWhite(0.20),
            PlotThemeWhite(1.0), PlotThemeWhite(0.02),
            PlotThemeWhite(0.02), PlotThemeWhite(0.48),
            false, true
        };
        style.markFill = PlotThemeWhite(0.02);
        style.markStroke = PlotThemeWhite(0.02);
        style.selectedMarkFill = PlotThemeWhite(0.02);
        style.selectedMarkStroke = PlotThemeWhite(0.55);
        style.auxiliary = PlotThemeWhite(0.25);
        style.reference = PlotThemeWhite(0.35);
        style.smooth = PlotThemeWhite(0.02);
        style.coordinatedMarks = true;
        style.crossUnselectedMarks = true;
        style.squareSelectedMarks = true;
        style.selectedMarkScale = 1.30;
        style.selectedMarkStrokeWidth = 1.6;
        return style;
    }
    if (theme == "garish") {
        // Deliberately exuberant, but with a single dark ink colour shared by
        // labels, axes, and mark outlines so the saturated surfaces remain
        // legible. Selection uses hue as well as an outline/size change.
        PlotThemeStyleSpec style{
            PlotThemeRGB(1.000, 0.310, 0.847), PlotThemeRGB(1.000, 0.953, 0.416),
            PlotThemeRGB(0.000, 0.847, 1.000, 0.32), PlotThemeRGB(0.416, 0.000, 1.000, 0.52),
            PlotThemeRGB(0.141, 0.000, 0.239), PlotThemeRGB(0.094, 0.000, 0.149), PlotThemeRGB(0.333, 0.125, 0.373),
            PlotThemeRGB(0.000, 0.847, 1.000, 0.82), PlotThemeRGB(0.141, 0.000, 0.239),
            PlotThemeRGB(1.000, 0.000, 0.361), PlotThemeRGB(0.220, 0.878, 0.000),
            true, true
        };
        style.markFill = PlotThemeRGB(0.000, 0.847, 1.000);
        style.markStroke = PlotThemeRGB(0.141, 0.000, 0.239);
        style.selectedMarkFill = PlotThemeRGB(1.000, 0.000, 0.361);
        style.selectedMarkStroke = PlotThemeRGB(0.141, 0.000, 0.239);
        style.auxiliary = PlotThemeRGB(0.220, 0.878, 0.000);
        style.reference = PlotThemeRGB(1.000, 0.420, 0.000);
        style.smooth = PlotThemeRGB(0.416, 0.000, 1.000);
        style.coordinatedMarks = true;
        style.selectedMarkScale = 1.24;
        style.selectedMarkStrokeWidth = 2.0;
        return style;
    }
    if (theme == "minimal") {
        return {
            PlotThemeWhite(1.0), PlotThemeWhite(1.0), PlotThemeWhite(0.965),
            PlotThemeWhite(0.900), PlotThemeWhite(0.120), PlotThemeWhite(0.090), PlotThemeWhite(0.360),
            PlotThemeWhite(0.925, 0.82), PlotThemeWhite(0.170, 0.92), PlotThemeRGB(0.000, 0.447, 0.698), PlotThemeRGB(0.902, 0.624, 0.000),
            false, false
        };
    }
    if (theme == "bw") {
        return {
            PlotThemeWhite(1.0), PlotThemeWhite(1.0), PlotThemeWhite(0.940),
            PlotThemeWhite(0.840), PlotThemeWhite(0.000), PlotThemeWhite(0.000), PlotThemeWhite(0.300),
            PlotThemeWhite(0.900, 0.88), PlotThemeWhite(0.000, 0.98), PlotThemeWhite(0.000), PlotThemeWhite(0.300),
            true, true
        };
    }
    if (theme == "gray") {
        return {
            PlotThemeWhite(0.965), PlotThemeWhite(0.925), PlotThemeWhite(1.000, 0.72),
            PlotThemeWhite(1.000, 0.98), PlotThemeWhite(0.180), PlotThemeWhite(0.100), PlotThemeWhite(0.360),
            PlotThemeWhite(0.820, 0.90), PlotThemeWhite(0.180, 0.95), PlotThemeWhite(0.250), PlotThemeWhite(0.580),
            true, true
        };
    }
    if (theme == "cowplot") {
        return {
            PlotThemeWhite(1.0), PlotThemeWhite(1.0), PlotThemeWhite(0.960),
            PlotThemeWhite(0.875), PlotThemeWhite(0.040), PlotThemeWhite(0.040), PlotThemeWhite(0.280),
            PlotThemeWhite(0.955, 0.70), PlotThemeWhite(0.050, 0.98), PlotThemeWhite(0.050), PlotThemeRGB(0.000, 0.447, 0.698),
            false, false
        };
    }
    if (theme == "ipsum") {
        return {
            PlotThemeRGB(0.982, 0.980, 0.960), PlotThemeRGB(0.996, 0.994, 0.976),
            PlotThemeRGB(0.910, 0.905, 0.875), PlotThemeRGB(0.840, 0.835, 0.800),
            PlotThemeRGB(0.210, 0.205, 0.185), PlotThemeRGB(0.140, 0.135, 0.120), PlotThemeRGB(0.420, 0.405, 0.360),
            PlotThemeRGB(0.905, 0.890, 0.825, 0.84), PlotThemeRGB(0.240, 0.230, 0.200, 0.94), PlotThemeRGB(0.000, 0.450, 0.520), PlotThemeRGB(0.760, 0.340, 0.130),
            false, false
        };
    }
    if (theme == "theme_tq") {
        return {
            PlotThemeRGB(0.955, 0.965, 0.975), PlotThemeWhite(1.0),
            PlotThemeRGB(0.920, 0.935, 0.950), PlotThemeRGB(0.815, 0.840, 0.865),
            PlotThemeRGB(0.090, 0.130, 0.170), PlotThemeRGB(0.060, 0.090, 0.120), PlotThemeRGB(0.330, 0.370, 0.410),
            PlotThemeRGB(0.785, 0.875, 0.935, 0.84), PlotThemeRGB(0.090, 0.220, 0.330, 0.95), PlotThemeRGB(0.000, 0.455, 0.700), PlotThemeRGB(0.835, 0.370, 0.000),
            true, false
        };
    }
    if (theme == "theme_modern") {
        return {
            PlotThemeRGB(0.070, 0.080, 0.095), PlotThemeRGB(0.105, 0.120, 0.140),
            PlotThemeRGB(0.180, 0.205, 0.235, 0.60), PlotThemeRGB(0.250, 0.285, 0.325, 0.78),
            PlotThemeRGB(0.785, 0.830, 0.870), PlotThemeRGB(0.925, 0.940, 0.955), PlotThemeRGB(0.650, 0.700, 0.745),
            PlotThemeRGB(0.170, 0.520, 0.720, 0.78), PlotThemeRGB(0.760, 0.880, 0.940, 0.95), PlotThemeRGB(0.000, 0.720, 0.820), PlotThemeRGB(0.930, 0.430, 0.230),
            true, false
        };
    }
    if (theme == "tufte") {
        return {
            PlotThemeWhite(1.0), PlotThemeWhite(1.0), PlotThemeWhite(0.980),
            PlotThemeWhite(0.910), PlotThemeWhite(0.230), PlotThemeWhite(0.100), PlotThemeWhite(0.380),
            PlotThemeWhite(1.000, 0.22), PlotThemeWhite(0.200, 0.82), PlotThemeWhite(0.120), PlotThemeRGB(0.835, 0.369, 0.000),
            false, false
        };
    }
    if (theme == "economist") {
        return {
            PlotThemeRGB(0.836, 0.863, 0.886), PlotThemeRGB(0.836, 0.863, 0.886),
            PlotThemeRGB(1.000, 1.000, 1.000, 0.52), PlotThemeRGB(1.000, 1.000, 1.000, 0.88),
            PlotThemeRGB(0.160, 0.190, 0.210), PlotThemeRGB(0.080, 0.100, 0.115), PlotThemeRGB(0.300, 0.335, 0.360),
            PlotThemeRGB(0.700, 0.780, 0.835, 0.86), PlotThemeRGB(0.090, 0.140, 0.175, 0.95), PlotThemeRGB(0.914, 0.118, 0.149), PlotThemeRGB(0.000, 0.357, 0.522),
            true, false
        };
    }
    if (theme == "fivethirtyeight") {
        return {
            PlotThemeRGB(0.940, 0.940, 0.930), PlotThemeRGB(0.940, 0.940, 0.930),
            PlotThemeRGB(1.000, 1.000, 1.000, 0.58), PlotThemeRGB(1.000, 1.000, 1.000, 0.92),
            PlotThemeRGB(0.230, 0.240, 0.245), PlotThemeRGB(0.120, 0.130, 0.135), PlotThemeRGB(0.390, 0.400, 0.410),
            PlotThemeRGB(0.780, 0.800, 0.810, 0.82), PlotThemeRGB(0.260, 0.280, 0.290, 0.88), PlotThemeRGB(0.000, 0.498, 0.710), PlotThemeRGB(0.930, 0.310, 0.190),
            true, false
        };
    }
    return {
        PlotThemeRGB(0.965, 0.970, 0.972), PlotThemeRGB(0.992, 0.992, 0.988),
        PlotThemeRGB(0.925, 0.935, 0.940), PlotThemeRGB(0.850, 0.870, 0.880),
        PlotThemeRGB(0.190, 0.220, 0.240), PlotThemeRGB(0.090, 0.110, 0.125), PlotThemeRGB(0.300, 0.330, 0.350),
        PlotThemeRGB(0.870, 0.910, 0.940, 0.82), PlotThemeRGB(0.180, 0.210, 0.235, 0.95), PlotThemeRGB(0.000, 0.447, 0.698), PlotThemeRGB(0.902, 0.624, 0.000),
        true, true
    };
}

std::string FormatFixedDecimalsLabel()
{
    return "Fixed decimals";
}

std::string FormatAutoLabel()
{
    return "Auto";
}

std::string FormatResetRowColorTooltip()
{
    return "Remove explicit row color for the current selection";
}

std::string FormatNameColumnHeader()
{
    return "Name";
}

std::string FormatTypeColumnHeader()
{
    return "Type";
}

std::string FormatDecimalsColumnHeader()
{
    return "Decimals";
}

std::string FormatRoleColumnHeader()
{
    return "Default role (Experimental)";
}

std::string FormatDescriptionColumnHeader()
{
    return "Description";
}

std::string FormatDefaultResetColorMenuItemTitle()
{
    return "Default / Reset color";
}

static const PaletteColor kPaletteData[] = {
    {"orange",     0.902, 0.624, 0.000, 0.984, 0.902, 0.702, 0.902, 0.624, 0.000, false},
    {"blue",       0.000, 0.447, 0.698, 0.812, 0.910, 0.965, 0.000, 0.447, 0.698, true},
    {"green",      0.000, 0.620, 0.451, 0.800, 0.929, 0.890, 0.000, 0.620, 0.451, true},
    {"vermillion", 0.835, 0.369, 0.000, 0.953, 0.831, 0.761, 0.835, 0.369, 0.000, true},
    {"purple",     0.800, 0.475, 0.655, 0.945, 0.867, 0.918, 0.800, 0.475, 0.655, false},
    {"brown",      0.651, 0.463, 0.114, 0.843, 0.765, 0.639, 0.651, 0.463, 0.114, true},
    {"pink",       0.847, 0.106, 0.376, 0.973, 0.733, 0.816, 0.847, 0.106, 0.376, true},
    {"yellow",     0.941176, 0.894118, 0.258824, 0.988, 0.965, 0.741, 0.941176, 0.894118, 0.258824, false}
};
static const size_t kPaletteCount = sizeof(kPaletteData) / sizeof(kPaletteData[0]);

const std::vector<PaletteColor>& PaletteColors()
{
    static const std::vector<PaletteColor> colors(kPaletteData, kPaletteData + kPaletteCount);
    return colors;
}

std::optional<PaletteColor> FindPaletteColor(const std::string &name)
{
    for (const PaletteColor &c : PaletteColors()) {
        if (name == c.name) {
            return c;
        }
    }
    return std::nullopt;
}

const PaletteColor* PaletteColorAtIndex(size_t index)
{
    const auto &colors = PaletteColors();
    return colors.empty() ? nullptr : &colors[index % colors.size()];
}

std::string PaletteColorNameAtIndex(size_t index)
{
    const auto &colors = PaletteColors();
    if (colors.empty()) {
        return "black";
    }
    return colors[index % colors.size()].name;
}

PlotRGBA PlotColorForName(const std::string &name, double alpha)
{
    std::string lowered = name;
    std::transform(lowered.begin(), lowered.end(), lowered.begin(),
                   [](unsigned char ch) { return static_cast<char>(std::tolower(ch)); });
    if (lowered == "white") {
        return PlotThemeWhite(1.0, alpha);
    }
    if (lowered == "black") {
        return PlotThemeWhite(0.0, alpha);
    }
    if (lowered == "red") {
        // A literal semantic red is used to distinguish rows whose model
        // inputs were directly imputed from ordinary fitted-value
        // propagation.  It must not fall through to the unknown-colour ink.
        return PlotThemeRGB(0.835, 0.050, 0.050, alpha);
    }
    if (lowered == "gray" || lowered == "grey") {
        return PlotThemeWhite(0.45, alpha);
    }
    auto color = FindPaletteColor(name);
    if (!color) {
        return PlotThemeRGB(0.070, 0.080, 0.085, alpha);
    }
    return PlotThemeRGB(color->baseR, color->baseG, color->baseB, alpha);
}

PlotRGBA PlotColorForNameOrHex(const std::string &nameOrHex, double alpha)
{
    std::string lowered = nameOrHex;
    std::transform(lowered.begin(), lowered.end(), lowered.begin(),
                   [](unsigned char ch) { return static_cast<char>(std::tolower(ch)); });
    if (lowered == "white") {
        return PlotThemeWhite(1.0, alpha);
    }
    if (lowered == "black") {
        return PlotThemeWhite(0.0, alpha);
    }
    auto color = FindPaletteColor(nameOrHex);
    if (color) {
        return PlotThemeRGB(color->baseR, color->baseG, color->baseB, alpha);
    }
    double r = 0.0;
    double g = 0.0;
    double b = 0.0;
    if (ParseHexRGB(nameOrHex, &r, &g, &b)) {
        return PlotThemeRGB(r, g, b, alpha);
    }
    return PlotColorForName(nameOrHex, alpha);
}

PlotRGBA PlotLightColorForNameOrHex(const std::string &nameOrHex, double alpha)
{
    if (auto color = FindPaletteColor(nameOrHex)) {
        return PlotThemeRGB(color->lightR, color->lightG, color->lightB, alpha);
    }
    const PlotRGBA base = PlotColorForNameOrHex(nameOrHex, 1.0);
    constexpr double whiteFraction = 0.72;
    return PlotThemeRGB(
        base.r + (1.0 - base.r) * whiteFraction,
        base.g + (1.0 - base.g) * whiteFraction,
        base.b + (1.0 - base.b) * whiteFraction,
        alpha);
}

PlotRGBA PlotSelectedColorForNameOrHex(const std::string &nameOrHex, double alpha)
{
    auto color = FindPaletteColor(nameOrHex);
    if (color) {
        return PlotThemeRGB(color->selectedR, color->selectedG, color->selectedB, alpha);
    }
    return PlotColorForNameOrHex(nameOrHex, alpha);
}

bool ParseHexColorByte(const std::string &text, size_t offset, double *value)
{
    if (!value || offset + 1 >= text.size()) {
        return false;
    }
    unsigned int byte = 0;
    if (std::sscanf(text.substr(offset, 2).c_str(), "%02x", &byte) != 1) {
        return false;
    }
    *value = (double)byte / 255.0;
    return true;
}

bool ParseHexRGB(const std::string &text, double *r, double *g, double *b)
{
    if (!r || !g || !b || text.size() != 7 || text[0] != '#') {
        return false;
    }
    return ParseHexColorByte(text, 1, r) &&
        ParseHexColorByte(text, 3, g) &&
        ParseHexColorByte(text, 5, b);
}

WindowFrame WindowPlacementNextFrame(WindowPlacementState &state,
                                     double width, double height,
                                     double screenWidth, double screenHeight,
                                     double margin, double gap)
{
    double x = state.nextX;
    double top = screenHeight - state.nextY;
    double y = top - height;

    if (x + width > screenWidth - margin) {
        state.nextX = 40;
        state.nextY += std::max(state.rowHeight + (int)gap, (int)height + (int)gap);
        state.rowHeight = 0;
        x = state.nextX;
        top = screenHeight - state.nextY;
        y = top - height;
    }
    if (y < margin) {
        state.nextX = 40;
        state.nextY = 60;
        state.rowHeight = 0;
        x = state.nextX;
        y = screenHeight - state.nextY - height;
    }

    state.nextX += (int)(width + gap);
    state.rowHeight = std::max(state.rowHeight, (int)height);
    return {x, y, width, height};
}

void WindowPlacementReset(WindowPlacementState &state, int startX, int startY)
{
    state.nextX = startX;
    state.nextY = startY;
    state.rowHeight = 0;
}

std::string SelectedColorForGroup(const std::string &group,
                                  const std::map<std::string, std::string> &groupSelectedColors)
{
    auto it = groupSelectedColors.find(group);
    if (it == groupSelectedColors.end()) {
        return "black";
    }
    return it->second;
}

std::string PointColorForRow(const std::string &group, int row,
                             const std::map<std::string, std::map<int, std::string>> &groupPointColors)
{
    auto git = groupPointColors.find(group);
    if (git == groupPointColors.end()) {
        return "";
    }
    auto rit = git->second.find(row);
    if (rit == git->second.end()) {
        return "";
    }
    return rit->second;
}

std::string DisplayColorForPoint(const std::string &group, int row,
                                 const std::map<std::string, std::map<int, std::string>> &groupPointColors)
{
    std::string colorName = PointColorForRow(group, row, groupPointColors);
    return colorName.empty() ? "black" : colorName;
}

} // namespace core
} // namespace rlispstat
