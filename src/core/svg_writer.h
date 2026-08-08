#ifndef RLISPSTAT_CORE_SVG_WRITER_H
#define RLISPSTAT_CORE_SVG_WRITER_H

#include "export_model.h"
#include "plot_geometry.h"

#include <sstream>
#include <string>
#include <vector>

namespace rlispstat {
namespace core {

struct SvgPaint {
    std::string fill = "none";
    std::string stroke = "none";
    double strokeWidth = 1.0;
    double opacity = 1.0;
    std::vector<double> dashPattern;
};

struct SvgTextStyle {
    std::string fill = "#111111";
    std::string fontFamily = "Arial, Helvetica, sans-serif";
    double fontSize = 12.0;
    int fontWeight = 400;
    bool italic = false;
    std::string anchor = "start";
};

class SvgWriter {
public:
    explicit SvgWriter(const ExportDimensions &dimensions);

    bool valid() const;
    const ExportDimensions &dimensions() const;
    std::string finish();

    void rectangle(double x, double y, double width, double height,
                   const SvgPaint &paint, double radius = 0.0);
    void line(double x1, double y1, double x2, double y2,
              const SvgPaint &paint);
    void ellipse(double centerX, double centerY, double radiusX, double radiusY,
                 const SvgPaint &paint);
    void path(const std::string &commands, const SvgPaint &paint);
    void text(double x, double y, const std::string &value,
              const SvgTextStyle &style, double rotationDegrees = 0.0);
    std::string beginClipRect(double x, double y, double width, double height);
    void endClip();

    static std::string EscapeText(const std::string &value);
    static std::string EscapeAttribute(const std::string &value);

private:
    void appendPaint(const SvgPaint &paint);
    bool finite(double value) const;
    std::string number(double value) const;

    ExportDimensions dimensions_;
    std::ostringstream output_;
    bool valid_ = true;
    bool finished_ = false;
    int clipCounter_ = 0;
    int openClips_ = 0;
};

struct SvgTableDocument {
    ExportDimensions dimensions;
    std::string svg;
};

// Self-contained vector rendering of an immutable PlotModel snapshot.  This
// deliberately accepts only portable value types; it never consults a live
// dataset, selection model, platform view, or R session.
SvgTableDocument BuildSvgPlotSnapshotDocument(const PlotModel &model,
                                              const std::string &themeName,
                                              const std::string &title,
                                              double widthPoints = 720.0,
                                              double heightPoints = 520.0);

// Logical table geometry shared by screen/PDF/PNG views and vector backends.
// Coordinates are PostScript points and contain no AppKit or Win32 types.
struct VectorTableLayout {
    ExportDimensions dimensions;
    std::string title;
    std::vector<std::vector<std::string>> rows;
    std::vector<double> columnWidths;
    std::vector<double> columnStarts;
    double margin = 24.0;
    double titleHeight = 18.0;
    double tableTop = 42.0;
    double rowHeight = 24.0;
};

VectorTableLayout BuildVectorTableLayout(const std::string &title,
                                         const std::string &tabDelimitedText,
                                         double maximumWidthPoints = 1200.0);

// Builds a self-contained, genuinely vector table from tab-delimited text.
// The first non-empty row is treated as the header.  The document contains
// selectable text and vector rules only; it never embeds a bitmap.
SvgTableDocument BuildSvgTableDocument(const std::string &title,
                                       const std::string &tabDelimitedText,
                                       double maximumWidthPoints = 1200.0);

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_SVG_WRITER_H
