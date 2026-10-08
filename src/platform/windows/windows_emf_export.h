#ifndef RLISPSTAT_PLATFORM_WINDOWS_EMF_EXPORT_H
#define RLISPSTAT_PLATFORM_WINDOWS_EMF_EXPORT_H

#include "../../core/export_model.h"
#include "../../core/plot_geometry.h"
#include "../../core/svg_writer.h"

#include <string>

namespace rlispstat {
namespace platform {
namespace windows {

bool CopyPlotAsEnhancedMetafile(
    const core::PlotModel &plot,
    const core::ExportDimensions &dimensions,
    std::string *error = nullptr);

// Publishes a native Enhanced Metafile for Office together with the canonical
// SVG payload. includeSvgText additionally exposes the SVG source as text.
bool CopyPlotWithOfficeClipboardFormats(
    const core::PlotModel &plot,
    const core::ExportDimensions &dimensions,
    const std::string &svg,
    bool includeSvgText,
    std::string *error = nullptr);

// Uses the same portable VectorTableLayout consumed by macOS PDF/PNG and SVG.
bool CopyTableAsEnhancedMetafile(
    const std::string &title,
    const std::string &tabDelimitedText,
    std::string *error = nullptr);

} // namespace windows
} // namespace platform
} // namespace rlispstat

#endif // RLISPSTAT_PLATFORM_WINDOWS_EMF_EXPORT_H
