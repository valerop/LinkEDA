#include "export_model.h"

#include <algorithm>
#include <cctype>
#include <set>

namespace rlispstat {
namespace core {

namespace {

void AddIf(std::vector<ExportMenuAction> &actions,
           const ExportCapabilities &capabilities,
           ExportCapability capability,
           const char *identifier,
           const char *title,
           const char *command,
           bool separatorBefore = false)
{
    if (!capabilities.has(capability)) return;
    actions.push_back({capability, identifier, title, command, separatorBefore});
}

} // namespace

ExportCapabilities StandardVisualExportCapabilities(ExportPlatform platform)
{
    ExportCapabilities result;
    result.supported = {
        ExportCapability::CopySvg,
        ExportCapability::CopyPng,
        ExportCapability::SaveSvg,
        ExportCapability::SavePdf,
        ExportCapability::SavePng
    };
    if (platform == ExportPlatform::MacOS) {
        result.supported.insert(ExportCapability::CopyPdf);
        result.supported.insert(ExportCapability::CopyVectorNative);
    } else if (platform == ExportPlatform::Windows) {
        result.supported.insert(ExportCapability::CopyVectorNative);
    }
    return result;
}

ExportCapabilities StandardTableExportCapabilities(ExportPlatform platform,
                                                   bool supportsApaPdf)
{
    ExportCapabilities result = StandardVisualExportCapabilities(platform);
    result.supported.insert(ExportCapability::CopyFormattedText);
    result.supported.insert(ExportCapability::CopyTabDelimitedText);
    result.supported.insert(ExportCapability::SaveCsv);
    result.supported.insert(ExportCapability::SaveDisplayedPdf);
    if (supportsApaPdf) result.supported.insert(ExportCapability::SaveApaPdf);
    return result;
}

std::vector<ExportMenuAction> BuildExportMenuActions(
    const ExportCapabilities &capabilities,
    ExportPlatform platform)
{
    std::vector<ExportMenuAction> actions;
    if (platform == ExportPlatform::Windows) {
        AddIf(actions, capabilities, ExportCapability::CopyVectorNative,
              "copy_emf", "Copy as Enhanced Metafile", "COPY_EMF");
    } else if (platform == ExportPlatform::MacOS) {
        AddIf(actions, capabilities, ExportCapability::CopyPdf,
              "copy_pdf", "Copy as PDF", "COPY_PDF");
    }
    AddIf(actions, capabilities, ExportCapability::CopySvg,
          "copy_svg", "Copy as SVG", "COPY_SVG");
    AddIf(actions, capabilities, ExportCapability::CopyPng,
          "copy_png", "Copy as PNG", "COPY_PNG");

    const bool hasCopy = !actions.empty();
    AddIf(actions, capabilities, ExportCapability::SaveSvg,
          "save_svg", "Save as SVG...", "SAVE_SVG", hasCopy);
    AddIf(actions, capabilities, ExportCapability::SavePdf,
          "save_pdf", "Save as PDF...", "SAVE_PDF");
    AddIf(actions, capabilities, ExportCapability::SavePng,
          "save_png", "Save as PNG...", "SAVE_PNG");

    const bool hasVisual = !actions.empty();
    AddIf(actions, capabilities, ExportCapability::CopyFormattedText,
          "copy_formatted_text", "Copy as formatted text", "COPY_FORMATTED_TEXT", hasVisual);
    AddIf(actions, capabilities, ExportCapability::CopyTabDelimitedText,
          "copy_tab_text", "Copy as tab-delimited text", "COPY_TAB_DELIMITED_TEXT");
    AddIf(actions, capabilities, ExportCapability::SaveCsv,
          "save_csv", "CSV...", "SAVE_CSV", hasVisual && !capabilities.has(ExportCapability::CopyFormattedText));
    AddIf(actions, capabilities, ExportCapability::SaveApaPdf,
          "save_apa_pdf", "PDF — APA 7 style...", "SAVE_APA_PDF");
    AddIf(actions, capabilities, ExportCapability::SaveDisplayedPdf,
          "save_displayed_pdf", "PDF — As displayed...", "SAVE_DISPLAYED_PDF");
    return actions;
}

std::vector<ExportAuditIssue> AuditExportSurfaceRegistry(
    const std::vector<ExportSurfaceRegistration> &surfaces,
    ExportPlatform platform)
{
    std::vector<ExportAuditIssue> issues;
    std::set<std::string> identifiers;
    for (const ExportSurfaceRegistration &surface : surfaces) {
        if (surface.identifier.empty()) {
            issues.push_back({surface.identifier, "Export surface has no identifier."});
            continue;
        }
        if (!identifiers.insert(surface.identifier).second) {
            issues.push_back({surface.identifier, "Export surface is registered more than once."});
        }
        if ((surface.kind == ExportSurfaceKind::Plot || surface.kind == ExportSurfaceKind::Table) &&
            !surface.capabilities.has(ExportCapability::SaveSvg)) {
            issues.push_back({surface.identifier, "Visual export surface does not provide Save as SVG."});
        }
        if ((surface.kind == ExportSurfaceKind::Plot || surface.kind == ExportSurfaceKind::Table) &&
            platform == ExportPlatform::Windows &&
            !surface.capabilities.has(ExportCapability::CopyVectorNative)) {
            issues.push_back({surface.identifier, "Windows visual surface does not provide EMF copy."});
        }
        const auto actions = BuildExportMenuActions(surface.capabilities, platform);
        std::set<std::string> actionIds;
        for (const ExportMenuAction &action : actions) {
            if (!actionIds.insert(action.identifier).second) {
                issues.push_back({surface.identifier, "Export menu contains a duplicate action."});
            }
            if (action.command.empty()) {
                issues.push_back({surface.identifier, "Export action has no command."});
            }
        }
    }
    return issues;
}

std::vector<ExportSurfaceRegistration> DefaultExportSurfaceRegistry(ExportPlatform platform)
{
    const ExportCapabilities visual = StandardVisualExportCapabilities(platform);
    const ExportCapabilities table = StandardTableExportCapabilities(platform, true);
    std::vector<ExportSurfaceRegistration> result;
    for (const char *identifier : {
             "scatterplot", "dot-plot", "boxplot", "histogram", "bar-chart",
             "time-series", "scatterplot-matrix", "trellis-plot", "forest-plot",
             "dendrogram", "model-diagnostic", "dimensionality-plot"}) {
        result.push_back({identifier, ExportSurfaceKind::Plot, visual});
    }
    for (const char *identifier : {
             "general-linear-model", "generalized-linear-model", "binary-regression",
             "compare-means", "anova", "pairwise-comparisons",
             "generalized-model-comparison", "regression-model-comparison",
             "linear-model-trellis", "contingency-table", "table-1",
             "descriptives", "mixed-model"}) {
        result.push_back({identifier, ExportSurfaceKind::Table, table});
    }
    ExportCapabilities data;
    data.supported = {ExportCapability::CopyTabDelimitedText, ExportCapability::SaveCsv};
    result.push_back({"data-sheet", ExportSurfaceKind::Data, data});
    return result;
}

std::string SafeExportBaseName(const std::string &value, const std::string &fallback)
{
    std::string output;
    output.reserve(std::min<std::size_t>(value.size(), 80));
    bool pendingDash = false;
    for (unsigned char ch : value) {
        if (output.size() >= 80) break;
        if (std::isalnum(ch) || ch >= 128) {
            if (pendingDash && !output.empty()) output.push_back('-');
            output.push_back(static_cast<char>(ch));
            pendingDash = false;
        } else if (ch == '-' || ch == '_' || std::isspace(ch)) {
            pendingDash = !output.empty();
        }
    }
    while (!output.empty() && output.back() == '-') output.pop_back();
    return output.empty() ? fallback : output;
}

} // namespace core
} // namespace rlispstat
