#ifndef RLISPSTAT_CORE_EXPORT_MODEL_H
#define RLISPSTAT_CORE_EXPORT_MODEL_H

#include <set>
#include <string>
#include <vector>

namespace rlispstat {
namespace core {

enum class ExportPlatform {
    MacOS,
    Windows,
    Linux
};

enum class ExportCapability {
    CopyVectorNative,
    CopySvg,
    CopyPdf,
    CopyPng,
    SaveSvg,
    SavePdf,
    SavePng,
    CopyFormattedText,
    CopyTabDelimitedText,
    SaveCsv,
    SaveApaPdf,
    SaveDisplayedPdf,
    ShowRCode,
    ShowRPublicationCode
};

enum class ExportSurfaceKind {
    Plot,
    Table,
    Data
};

struct ExportCapabilities {
    std::set<ExportCapability> supported;

    bool has(ExportCapability capability) const {
        return supported.find(capability) != supported.end();
    }
};

struct ExportMenuAction {
    ExportCapability capability;
    std::string identifier;
    std::string title;
    std::string command;
    bool separatorBefore = false;
    std::string categoryIdentifier;
    std::string categoryTitle;
};

struct ExportDimensions {
    double widthPoints = 504.0;
    double heightPoints = 360.0;
    double rasterScale = 2.0;
};

struct ExportSurfaceRegistration {
    std::string identifier;
    ExportSurfaceKind kind = ExportSurfaceKind::Plot;
    ExportCapabilities capabilities;
};

struct ExportAuditIssue {
    std::string surface;
    std::string message;
};

ExportCapabilities StandardVisualExportCapabilities(ExportPlatform platform);
ExportCapabilities StandardTableExportCapabilities(ExportPlatform platform,
                                                   bool supportsApaPdf = false);
std::vector<ExportMenuAction> BuildExportMenuActions(
    const ExportCapabilities &capabilities,
    ExportPlatform platform);
// R Code is a nested, extensible export category. Callers supply availability
// from the selected object's stored provenance/specification.
std::vector<ExportMenuAction> BuildRCodeExportMenuActions(bool hasRCode,
                                                          bool hasPublicationCode);
std::vector<ExportAuditIssue> AuditExportSurfaceRegistry(
    const std::vector<ExportSurfaceRegistration> &surfaces,
    ExportPlatform platform);
std::vector<ExportSurfaceRegistration> DefaultExportSurfaceRegistry(
    ExportPlatform platform);
std::string SafeExportBaseName(const std::string &value,
                               const std::string &fallback = "rlispstat-export");
// Convert an RFC-style CSV report to tab-delimited display text without
// splitting quoted labels or warnings at their internal commas.
std::string CsvReportToTabDelimited(const std::string &csv);

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_EXPORT_MODEL_H
