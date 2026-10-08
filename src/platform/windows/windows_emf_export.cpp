#include "windows_emf_export.h"
#include "../../core/format_model.h"
#include "../../core/scatterplot_model.h"

#ifdef _WIN32

#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <algorithm>
#include <cmath>
#include <vector>
#include <windows.h>

namespace rlispstat {
namespace platform {
namespace windows {

namespace {

std::wstring Utf8ToWide(const std::string &value)
{
    if (value.empty()) return std::wstring();
    const int count = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS,
        value.data(), static_cast<int>(value.size()), nullptr, 0);
    if (count <= 0) return std::wstring(value.begin(), value.end());
    std::wstring result(static_cast<std::size_t>(count), L'\0');
    MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, value.data(),
        static_cast<int>(value.size()), &result[0], count);
    return result;
}

void DrawTextUtf8(HDC dc, const std::string &text, int x, int y, UINT alignment)
{
    const std::wstring wide = Utf8ToWide(text);
    SetTextAlign(dc, alignment | TA_TOP);
    TextOutW(dc, x, y, wide.c_str(), static_cast<int>(wide.size()));
}

void FillVectorRect(HDC dc, const RECT &rect, HBRUSH brush)
{
    // FillRect may be encoded by some GDI drivers as EMR_BITBLT, which makes
    // an otherwise vector-only EMF contain a raster operation.  Rectangle
    // with a NULL_PEN records an explicit EMR_RECTANGLE instead.
    HGDIOBJ oldBrush = SelectObject(dc, brush);
    HGDIOBJ oldPen = SelectObject(dc, GetStockObject(NULL_PEN));
    Rectangle(dc, rect.left, rect.top, rect.right, rect.bottom);
    SelectObject(dc, oldPen);
    SelectObject(dc, oldBrush);
}

bool SetClipboardBytes(UINT format, const void *bytes, std::size_t size)
{
    HGLOBAL data = GlobalAlloc(GMEM_MOVEABLE, size);
    if (!data) return false;
    void *destination = GlobalLock(data);
    if (!destination) {
        GlobalFree(data);
        return false;
    }
    CopyMemory(destination, bytes, size);
    GlobalUnlock(data);
    if (!SetClipboardData(format, data)) {
        GlobalFree(data);
        return false;
    }
    return true;
}

bool FinishAndCopyMetafile(HDC metafile, const std::string *svg,
                           bool includeSvgText, std::string *error)
{
    HENHMETAFILE handle = CloseEnhMetaFile(metafile);
    if (!handle) {
        if (error) *error = "The Enhanced Metafile could not be finalized.";
        return false;
    }
    bool clipboardOpen = false;
    for (int attempt = 0; attempt < 20 && !clipboardOpen; ++attempt) {
        clipboardOpen = OpenClipboard(nullptr) != FALSE;
        if (!clipboardOpen) Sleep(5);
    }
    if (!clipboardOpen) {
        DeleteEnhMetaFile(handle);
        if (error) *error = "The Windows clipboard is unavailable.";
        return false;
    }
    bool copied = false;
    if (EmptyClipboard()) {
        copied = SetClipboardData(CF_ENHMETAFILE, handle) != nullptr;
        if (copied && svg && !svg->empty()) {
            const UINT svgFormat = RegisterClipboardFormatW(L"image/svg+xml");
            if (svgFormat != 0) {
                std::string terminated = *svg;
                terminated.push_back('\0');
                (void)SetClipboardBytes(svgFormat, terminated.data(), terminated.size());
            }
            if (includeSvgText) {
                std::wstring wide = Utf8ToWide(*svg);
                wide.push_back(L'\0');
                (void)SetClipboardBytes(CF_UNICODETEXT, wide.data(),
                    wide.size() * sizeof(wchar_t));
            }
        }
    }
    CloseClipboard();
    if (!copied) {
        DeleteEnhMetaFile(handle);
        if (error) *error = "The Enhanced Metafile could not be placed on the clipboard.";
        return false;
    }
    // After successful SetClipboardData, ownership belongs to the system.
    return true;
}

} // namespace

static bool CopyPlotClipboardImpl(const core::PlotModel &plot,
                                  const core::ExportDimensions &dimensions,
                                  const std::string *svg,
                                  bool includeSvgText,
                                  std::string *error)
{
    if (!std::isfinite(dimensions.widthPoints) || !std::isfinite(dimensions.heightPoints) ||
        dimensions.widthPoints <= 0.0 || dimensions.heightPoints <= 0.0) {
        if (error) *error = "Invalid export dimensions.";
        return false;
    }
    HDC reference = GetDC(nullptr);
    if (!reference) {
        if (error) *error = "A reference graphics context could not be created.";
        return false;
    }
    // An EMF frame is expressed in 0.01 mm. One PostScript point is 1/72 inch.
    RECT frame{};
    frame.right = static_cast<LONG>(std::llround(dimensions.widthPoints * 2540.0 / 72.0));
    frame.bottom = static_cast<LONG>(std::llround(dimensions.heightPoints * 2540.0 / 72.0));
    HDC metafile = CreateEnhMetaFileW(reference, nullptr, &frame,
        L"rlispstat\0Vector plot\0\0");
    ReleaseDC(nullptr, reference);
    if (!metafile) {
        if (error) *error = "The Enhanced Metafile could not be created.";
        return false;
    }

    const int width = std::max(1, static_cast<int>(std::llround(dimensions.widthPoints)));
    const int height = std::max(1, static_cast<int>(std::llround(dimensions.heightPoints)));
    SetMapMode(metafile, MM_ANISOTROPIC);
    SetWindowExtEx(metafile, width, height, nullptr);
    SetViewportExtEx(metafile, width, height, nullptr);
    SetBkMode(metafile, TRANSPARENT);
    SetTextColor(metafile, RGB(28, 31, 34));

    HBRUSH white = CreateSolidBrush(RGB(255, 255, 255));
    RECT background{0, 0, width, height};
    FillVectorRect(metafile, background, white);
    DeleteObject(white);

    HFONT titleFont = CreateFontW(-15, 0, 0, 0, FW_SEMIBOLD, FALSE, FALSE, FALSE,
        DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, CLEARTYPE_QUALITY,
        DEFAULT_PITCH | FF_SWISS, L"Arial");
    HFONT labelFont = CreateFontW(-11, 0, 0, 0, FW_NORMAL, FALSE, FALSE, FALSE,
        DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, CLEARTYPE_QUALITY,
        DEFAULT_PITCH | FF_SWISS, L"Arial");
    HGDIOBJ oldFont = SelectObject(metafile, titleFont);
    DrawTextUtf8(metafile, plot.title, width / 2, 12, TA_CENTER);
    SelectObject(metafile, labelFont);

    const core::Rect plotRect{58.0, 42.0,
        std::max(20.0, dimensions.widthPoints - 82.0),
        std::max(20.0, dimensions.heightPoints - 92.0)};
    std::vector<core::Point> dataPoints;
    dataPoints.reserve(plot.points.size());
    for (const core::DataPoint &point : plot.points) dataPoints.push_back({point.x, point.y});
    const core::DataViewport viewport = core::DataViewportForPoints(dataPoints);
    HPEN gridPen = CreatePen(PS_SOLID, 1, RGB(220, 224, 228));
    HPEN axisPen = CreatePen(PS_SOLID, 1, RGB(48, 54, 58));
    HGDIOBJ oldPen = SelectObject(metafile, gridPen);
    for (int tick = 0; tick <= 5; ++tick) {
        const int x = static_cast<int>(std::llround(plotRect.x + plotRect.width * tick / 5.0));
        const int y = static_cast<int>(std::llround(plotRect.y + plotRect.height * tick / 5.0));
        MoveToEx(metafile, x, static_cast<int>(plotRect.y), nullptr);
        LineTo(metafile, x, static_cast<int>(plotRect.y + plotRect.height));
        MoveToEx(metafile, static_cast<int>(plotRect.x), y, nullptr);
        LineTo(metafile, static_cast<int>(plotRect.x + plotRect.width), y);
    }
    SelectObject(metafile, axisPen);
    MoveToEx(metafile, static_cast<int>(plotRect.x), static_cast<int>(plotRect.y), nullptr);
    LineTo(metafile, static_cast<int>(plotRect.x), static_cast<int>(plotRect.y + plotRect.height));
    LineTo(metafile, static_cast<int>(plotRect.x + plotRect.width), static_cast<int>(plotRect.y + plotRect.height));

    SaveDC(metafile);
    IntersectClipRect(metafile, static_cast<int>(plotRect.x), static_cast<int>(plotRect.y),
        static_cast<int>(plotRect.x + plotRect.width), static_cast<int>(plotRect.y + plotRect.height));
    for (const core::ScatterplotSmoothCurveDrawItem &curve :
         core::BuildSmoothCurveDrawItems(plot.smoothCurves, viewport, plotRect)) {
        const core::PlotRGBA rgba = curve.colorName.empty()
            ? core::PlotColorForNameOrHex("black")
            : core::PlotColorForNameOrHex(curve.colorName);
        HPEN curvePen = CreatePen(curve.dashed ? PS_DASH : PS_SOLID,
            std::max(1, static_cast<int>(std::llround(curve.lineWidth))),
            RGB(static_cast<int>(rgba.r * 255.0), static_cast<int>(rgba.g * 255.0),
                static_cast<int>(rgba.b * 255.0)));
        HGDIOBJ previous = SelectObject(metafile, curvePen);
        if (!curve.points.empty()) {
            MoveToEx(metafile, static_cast<int>(std::llround(curve.points.front().x)),
                     static_cast<int>(std::llround(curve.points.front().y)), nullptr);
            for (std::size_t index = 1; index < curve.points.size(); ++index)
                LineTo(metafile, static_cast<int>(std::llround(curve.points[index].x)),
                       static_cast<int>(std::llround(curve.points[index].y)));
        }
        SelectObject(metafile, previous);
        DeleteObject(curvePen);
    }
    for (const core::DataPoint &point : plot.points) {
        if (!std::isfinite(point.x) || !std::isfinite(point.y)) continue;
        const core::Point screen = core::DataToScreen({point.x, point.y}, viewport, plotRect, true);
        const int x = static_cast<int>(std::llround(screen.x));
        const int y = static_cast<int>(std::llround(screen.y));
        core::PlotRGBA rgba = core::PlotColorForNameOrHex("black");
        auto color = plot.frozenRowColors.find(point.row);
        if (color != plot.frozenRowColors.end() && !color->second.empty())
            rgba = core::PlotColorForNameOrHex(color->second);
        HPEN pointPen = CreatePen(PS_SOLID, 1,
            RGB(static_cast<int>(rgba.r * 255.0), static_cast<int>(rgba.g * 255.0),
                static_cast<int>(rgba.b * 255.0)));
        HBRUSH pointBrush = CreateSolidBrush(
            RGB(static_cast<int>(rgba.r * 255.0), static_cast<int>(rgba.g * 255.0),
                static_cast<int>(rgba.b * 255.0)));
        HGDIOBJ previousPen = SelectObject(metafile, pointPen);
        HGDIOBJ previousBrush = SelectObject(metafile, pointBrush);
        Ellipse(metafile, x - 3, y - 3, x + 4, y + 4);
        SelectObject(metafile, previousBrush);
        SelectObject(metafile, previousPen);
        DeleteObject(pointBrush);
        DeleteObject(pointPen);
    }
    RestoreDC(metafile, -1);
    DrawTextUtf8(metafile, plot.xLabel, width / 2, height - 24, TA_CENTER);
    DrawTextUtf8(metafile, plot.yLabel, 8, height / 2, TA_LEFT);

    SelectObject(metafile, oldPen);
    SelectObject(metafile, oldFont);
    DeleteObject(gridPen);
    DeleteObject(axisPen);
    DeleteObject(titleFont);
    DeleteObject(labelFont);

    return FinishAndCopyMetafile(metafile, svg, includeSvgText, error);
}

bool CopyPlotAsEnhancedMetafile(const core::PlotModel &plot,
                                const core::ExportDimensions &dimensions,
                                std::string *error)
{
    return CopyPlotClipboardImpl(plot, dimensions, nullptr, false, error);
}

bool CopyPlotWithOfficeClipboardFormats(const core::PlotModel &plot,
                                        const core::ExportDimensions &dimensions,
                                        const std::string &svg,
                                        bool includeSvgText,
                                        std::string *error)
{
    return CopyPlotClipboardImpl(plot, dimensions, &svg, includeSvgText, error);
}

bool CopyTableAsEnhancedMetafile(const std::string &title,
                                 const std::string &tabDelimitedText,
                                 std::string *error)
{
    const core::VectorTableLayout layout = core::BuildVectorTableLayout(title, tabDelimitedText);
    const int width = std::max(1, static_cast<int>(std::llround(layout.dimensions.widthPoints)));
    const int height = std::max(1, static_cast<int>(std::llround(layout.dimensions.heightPoints)));
    HDC reference = GetDC(nullptr);
    if (!reference) {
        if (error) *error = "A reference graphics context could not be created.";
        return false;
    }
    RECT frame{};
    frame.right = static_cast<LONG>(std::llround(layout.dimensions.widthPoints * 2540.0 / 72.0));
    frame.bottom = static_cast<LONG>(std::llround(layout.dimensions.heightPoints * 2540.0 / 72.0));
    HDC metafile = CreateEnhMetaFileW(reference, nullptr, &frame,
        L"rlispstat\0Vector table\0\0");
    ReleaseDC(nullptr, reference);
    if (!metafile) {
        if (error) *error = "The Enhanced Metafile could not be created.";
        return false;
    }
    SetMapMode(metafile, MM_ANISOTROPIC);
    SetWindowExtEx(metafile, width, height, nullptr);
    SetViewportExtEx(metafile, width, height, nullptr);
    SetBkMode(metafile, TRANSPARENT);
    SetTextColor(metafile, RGB(24, 24, 24));
    HBRUSH white = CreateSolidBrush(RGB(255, 255, 255));
    RECT background{0, 0, width, height};
    FillVectorRect(metafile, background, white);
    DeleteObject(white);

    HFONT titleFont = CreateFontW(-16, 0, 0, 0, FW_SEMIBOLD, FALSE, FALSE, FALSE,
        DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, CLEARTYPE_QUALITY,
        DEFAULT_PITCH | FF_SWISS, L"Arial");
    HFONT headerFont = CreateFontW(-11, 0, 0, 0, FW_SEMIBOLD, FALSE, FALSE, FALSE,
        DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, CLEARTYPE_QUALITY,
        DEFAULT_PITCH | FF_SWISS, L"Arial");
    HFONT bodyFont = CreateFontW(-11, 0, 0, 0, FW_NORMAL, FALSE, FALSE, FALSE,
        DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, CLEARTYPE_QUALITY,
        DEFAULT_PITCH | FF_SWISS, L"Arial");
    HPEN rulePen = CreatePen(PS_SOLID, 1, RGB(138, 138, 138));
    HGDIOBJ oldFont = SelectObject(metafile, titleFont);
    HGDIOBJ oldPen = SelectObject(metafile, rulePen);
    if (!layout.title.empty()) {
        DrawTextUtf8(metafile, layout.title, static_cast<int>(layout.margin),
                     static_cast<int>(layout.margin), TA_LEFT);
    }
    MoveToEx(metafile, static_cast<int>(layout.margin), static_cast<int>(layout.tableTop), nullptr);
    LineTo(metafile, width - static_cast<int>(layout.margin), static_cast<int>(layout.tableTop));
    for (std::size_t row = 0; row < layout.rows.size(); ++row) {
        const int y = static_cast<int>(std::llround(layout.tableTop + layout.rowHeight * row));
        if (row == 0) {
            HBRUSH headerBrush = CreateSolidBrush(RGB(242, 242, 242));
            RECT headerRect{static_cast<LONG>(layout.margin), y,
                width - static_cast<LONG>(layout.margin),
                y + static_cast<LONG>(layout.rowHeight)};
            FillVectorRect(metafile, headerRect, headerBrush);
            DeleteObject(headerBrush);
        }
        SelectObject(metafile, row == 0 ? headerFont : bodyFont);
        for (std::size_t column = 0; column < layout.columnWidths.size(); ++column) {
            const int x = static_cast<int>(std::llround(layout.columnStarts[column]));
            const int cellWidth = static_cast<int>(std::llround(layout.columnWidths[column]));
            const std::string value = column < layout.rows[row].size()
                ? layout.rows[row][column] : std::string();
            const int saved = SaveDC(metafile);
            IntersectClipRect(metafile, x + 1, y + 1, x + std::max(1, cellWidth - 1),
                              y + static_cast<int>(layout.rowHeight) - 1);
            DrawTextUtf8(metafile, value, x + 6, y + 5, TA_LEFT);
            RestoreDC(metafile, saved);
        }
        if (row == 0 || row + 1 == layout.rows.size()) {
            const int ruleY = y + static_cast<int>(layout.rowHeight);
            MoveToEx(metafile, static_cast<int>(layout.margin), ruleY, nullptr);
            LineTo(metafile, width - static_cast<int>(layout.margin), ruleY);
        }
    }
    SelectObject(metafile, oldFont);
    SelectObject(metafile, oldPen);
    DeleteObject(titleFont);
    DeleteObject(headerFont);
    DeleteObject(bodyFont);
    DeleteObject(rulePen);
    return FinishAndCopyMetafile(metafile, nullptr, false, error);
}

} // namespace windows
} // namespace platform
} // namespace rlispstat

#endif // _WIN32
