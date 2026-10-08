#ifndef RLISPSTAT_CORE_FORMAT_MODEL_H
#define RLISPSTAT_CORE_FORMAT_MODEL_H

#include <map>
#include <optional>
#include <string>
#include <vector>

namespace rlispstat {
namespace core {

std::string FormatDouble(double value, int digits = 4);
std::string FormatDoubleOrDash(double value, int digits = 4);
// Compact model-table formatting: at most three decimals, no insignificant
// trailing zeroes, and scientific notation only for values whose scale makes
// fixed notation misleading or too wide.
std::string FormatModelNumber(double value);
std::string FormatModelNumberOrDash(double value);
std::string FormatPValue(double p);
std::string FormatPercent(double p, int digits = 1);
std::string FormatPercentOrDash(double p, int digits = 1);
std::string FormatCorrelation(double r);
std::string FormatFactorLabel(double value);
std::string PlotWindowTitle(const std::string &title,
                             const std::string &yLabel,
                             const std::string &xLabel,
                             const std::string &group,
                             const std::string &interactionMode,
                             const std::string &selectionMode);
std::string PlotDerivedColumnSourceLabel(const std::string &title,
                                         const std::string &yLabel,
                                         const std::string &xLabel);
std::string PlotExportFilename(const std::string &plotId,
                               const std::string &format,
                               const std::string &prefix = "LinkEDA");
std::string PlotExportFileExtension(const std::string &format);
std::string PlotExportPanelTitle(const std::string &format);
std::string PlotExportFailedTitle();
std::string PlotExportFailedStatus();
std::string PlotCopyFailedTitle();
std::string PlotCopyFailedStatus();
std::string PlotThemeMessageTitle();
std::string PlotThemeUnknownStatus();
std::string SignificanceStars(double p);
bool PlotThemeIsValid(const std::string &theme);
std::string PlotThemeDisplayName(const std::string &theme);
std::vector<std::string> PlotThemeNames();
struct PlotRGBA {
    double r = 0.0;
    double g = 0.0;
    double b = 0.0;
    double a = 1.0;
};
struct PlotThemeStyleSpec {
    PlotRGBA background;
    PlotRGBA panel;
    PlotRGBA minorGrid;
    PlotRGBA majorGrid;
    PlotRGBA axis;
    PlotRGBA text;
    PlotRGBA mutedText;
    PlotRGBA geomFill;
    PlotRGBA geomStroke;
    PlotRGBA accent;
    PlotRGBA accent2;
    bool showMinorGrid = true;
    bool showPanelBorder = true;
    // Shared mark roles. Existing themes keep the historical defaults; themes
    // that opt in can coordinate point, selection, smooth, and reference colors
    // without plot-specific theme branches.
    PlotRGBA markFill{0.070, 0.080, 0.085, 1.0};
    PlotRGBA markStroke{0.020, 0.025, 0.028, 1.0};
    PlotRGBA selectedMarkFill{0.070, 0.080, 0.085, 1.0};
    PlotRGBA selectedMarkStroke{0.580, 0.580, 0.580, 1.0};
    PlotRGBA auxiliary{0.000, 0.447, 0.698, 1.0};
    PlotRGBA reference{0.480, 0.480, 0.480, 1.0};
    PlotRGBA smooth{0.070, 0.080, 0.085, 1.0};
    bool coordinatedMarks = false;
    bool hollowUnselectedMarks = false;
    bool crossUnselectedMarks = false;
    bool squareSelectedMarks = false;
    double selectedMarkScale = 1.0;
    double selectedMarkStrokeWidth = 1.5;
    bool showAxisTickMarks = false;
};
PlotRGBA PlotThemeRGB(double r, double g, double b, double a = 1.0);
PlotRGBA PlotThemeWhite(double w, double a = 1.0);
// Density shading accumulates opacity; uniform marks resolve the muted tone once.
PlotRGBA PlotMarkColor(PlotRGBA color, PlotRGBA panel, bool shadeOverlap, double alphaMultiplier = 1.0);
PlotThemeStyleSpec PlotThemeStyleForName(const std::string &theme);
PlotRGBA PlotColorForName(const std::string &name, double alpha = 1.0);
PlotRGBA PlotColorForNameOrHex(const std::string &nameOrHex, double alpha = 1.0);
PlotRGBA PlotLightColorForNameOrHex(const std::string &nameOrHex, double alpha = 1.0);
PlotRGBA PlotSelectedColorForNameOrHex(const std::string &nameOrHex, double alpha = 1.0);
std::string PlotNoActiveStatus();
std::string UnknownColorStatus();
std::string FormatFixedDecimalsLabel();
std::string FormatAutoLabel();
std::string FormatResetRowColorTooltip();
std::string FormatNameColumnHeader();
std::string FormatTypeColumnHeader();
std::string FormatDecimalsColumnHeader();
std::string FormatRoleColumnHeader();
std::string FormatDescriptionColumnHeader();
struct PaletteColor {
    const char *name;
    double baseR, baseG, baseB;
    double lightR, lightG, lightB;
    double selectedR, selectedG, selectedB;
    bool selectedTextWhite;
};

const std::vector<PaletteColor>& PaletteColors();
std::optional<PaletteColor> FindPaletteColor(const std::string &name);
const PaletteColor* PaletteColorAtIndex(size_t index);
std::string PaletteColorNameAtIndex(size_t index);

std::string SelectedColorForGroup(const std::string &group,
                                  const std::map<std::string, std::string> &groupSelectedColors);
std::string PointColorForRow(const std::string &group, int row,
                             const std::map<std::string, std::map<int, std::string>> &groupPointColors);
std::string DisplayColorForPoint(const std::string &group, int row,
                                 const std::map<std::string, std::map<int, std::string>> &groupPointColors);

struct WindowPlacementState {
    int nextX = 40;
    int nextY = 60;
    int rowHeight = 0;
};

struct WindowFrame {
    double x, y, width, height;
};

WindowFrame WindowPlacementNextFrame(WindowPlacementState &state,
                                     double width, double height,
                                     double screenWidth, double screenHeight,
                                     double margin = 40.0, double gap = 20.0);
void WindowPlacementReset(WindowPlacementState &state,
                          int startX = 40, int startY = 60);

std::string FormatDefaultResetColorMenuItemTitle();

bool ParseHexColorByte(const std::string &text, size_t offset, double *value);
bool ParseHexRGB(const std::string &text, double *r, double *g, double *b);

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_FORMAT_MODEL_H
