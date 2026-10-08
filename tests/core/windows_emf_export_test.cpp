#include "../../src/platform/windows/windows_emf_export.h"

#ifdef _WIN32

#include <cassert>
#include <cmath>
#include <windows.h>

namespace {

struct RecordCounts {
    int vector = 0;
    int raster = 0;
};

int CALLBACK CountRecords(HDC, HANDLETABLE *, const ENHMETARECORD *record,
                          int, LPARAM value)
{
    auto *counts = reinterpret_cast<RecordCounts *>(value);
    switch (record->iType) {
        case EMR_LINETO:
        case EMR_RECTANGLE:
        case EMR_ELLIPSE:
        case EMR_EXTTEXTOUTA:
        case EMR_EXTTEXTOUTW:
        case EMR_POLYLINE:
        case EMR_POLYGON:
            ++counts->vector;
            break;
        case EMR_BITBLT:
        case EMR_STRETCHBLT:
        case EMR_STRETCHDIBITS:
        case EMR_SETDIBITSTODEVICE:
            ++counts->raster;
            break;
        default:
            break;
    }
    return 1;
}

HENHMETAFILE ClipboardMetafileCopy()
{
    assert(OpenClipboard(nullptr));
    assert(IsClipboardFormatAvailable(CF_ENHMETAFILE));
    auto source = static_cast<HENHMETAFILE>(GetClipboardData(CF_ENHMETAFILE));
    HENHMETAFILE copy = source ? CopyEnhMetaFileW(source, nullptr) : nullptr;
    CloseClipboard();
    return copy;
}

void VerifyClipboardVector(double expectedAspect)
{
    HENHMETAFILE handle = ClipboardMetafileCopy();
    assert(handle != nullptr);
    ENHMETAHEADER header{};
    assert(GetEnhMetaFileHeader(handle, sizeof(header), &header) == sizeof(header));
    assert(header.dSignature == ENHMETA_SIGNATURE);
    assert(header.nBytes > sizeof(ENHMETAHEADER));
    const double width = static_cast<double>(header.rclFrame.right - header.rclFrame.left);
    const double height = static_cast<double>(header.rclFrame.bottom - header.rclFrame.top);
    assert(width > 0.0 && height > 0.0);
    assert(std::fabs(width / height - expectedAspect) < 0.03);
    RecordCounts counts;
    assert(EnumEnhMetaFile(nullptr, handle, CountRecords, &counts, nullptr));
    assert(counts.vector > 0);
    assert(counts.raster == 0);
    DeleteEnhMetaFile(handle);
}

} // namespace

int main()
{
    using namespace rlispstat;
    core::PlotModel plot;
    plot.title = "Vector − β";
    plot.xLabel = "x";
    plot.yLabel = "y";
    plot.points = {{1, 2, 1}, {2, 4, 2}, {3, 3, 3}};
    std::string error;
    const core::ExportDimensions dimensions{504, 360, 2};
    assert(platform::windows::CopyPlotAsEnhancedMetafile(plot, dimensions, &error));
    VerifyClipboardVector(504.0 / 360.0);

    plot.smoothCurves.push_back({core::SmoothCurveScope::ColorGroup, "#0078D4",
        {1.0, 2.0, 3.0}, {2.0, 3.5, 3.0}, true, ""});
    plot.frozenRowColors[1] = "#E83E8C";
    const std::string svg = "<?xml version=\"1.0\"?><svg xmlns=\"http://www.w3.org/2000/svg\"></svg>";
    assert(platform::windows::CopyPlotWithOfficeClipboardFormats(
        plot, dimensions, svg, true, &error));
    VerifyClipboardVector(504.0 / 360.0);
    assert(OpenClipboard(nullptr));
    assert(IsClipboardFormatAvailable(RegisterClipboardFormatW(L"image/svg+xml")));
    assert(IsClipboardFormatAvailable(CF_UNICODETEXT));
    CloseClipboard();

    assert(platform::windows::CopyTableAsEnhancedMetafile(
        "Table 1 — β", "Variable\tb\tp\nmpg\t0.041\t.723\ncyl\t—\t.002", &error));
    const core::VectorTableLayout table = core::BuildVectorTableLayout(
        "Table 1 — β", "Variable\tb\tp\nmpg\t0.041\t.723\ncyl\t—\t.002");
    VerifyClipboardVector(table.dimensions.widthPoints / table.dimensions.heightPoints);

    const DWORD before = GetGuiResources(GetCurrentProcess(), GR_GDIOBJECTS);
    for (int iteration = 0; iteration < 10; ++iteration) {
        assert(platform::windows::CopyPlotAsEnhancedMetafile(plot, dimensions, &error));
    }
    const DWORD after = GetGuiResources(GetCurrentProcess(), GR_GDIOBJECTS);
    assert(after <= before + 3);
    return 0;
}

#else

int main() { return 0; }

#endif
