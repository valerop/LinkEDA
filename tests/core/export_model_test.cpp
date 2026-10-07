#include "../../src/core/export_model.h"

#include <cassert>
#include <string>
#include <vector>

int main()
{
    using namespace rlispstat::core;

    const ExportCapabilities mac = StandardVisualExportCapabilities(ExportPlatform::MacOS);
    const auto macActions = BuildExportMenuActions(mac, ExportPlatform::MacOS);
    assert(macActions.size() == 6);
    assert(macActions[0].title == "Copy as PDF");
    assert(macActions[1].title == "Copy as SVG");
    assert(macActions[2].title == "Copy as PNG");
    assert(macActions[3].separatorBefore);
    assert(macActions[3].title == "Save as SVG...");
    assert(macActions[4].title == "Save as PDF...");
    assert(macActions[5].title == "Save as PNG...");

    const ExportCapabilities windows = StandardVisualExportCapabilities(ExportPlatform::Windows);
    const auto windowsActions = BuildExportMenuActions(windows, ExportPlatform::Windows);
    // Windows offers a rich SVG+PNG clipboard package for Office and keeps
    // explicit SVG/PNG commands.  The legacy point-only EMF renderer is not
    // advertised as a faithful plot copy.
    assert(windowsActions.front().title == "Copy for Office");
    assert(windowsActions.front().command == "COPY_RICH");
    assert(windowsActions[1].title == "Copy as SVG");
    assert(windowsActions[2].title == "Copy as PNG");
    for (const ExportMenuAction &action : windowsActions) {
        assert(action.identifier != "copy_emf");
        assert(action.title.find("PDF") == std::string::npos || action.identifier == "save_pdf");
    }

    const ExportCapabilities linux = StandardVisualExportCapabilities(ExportPlatform::Linux);
    const auto linuxActions = BuildExportMenuActions(linux, ExportPlatform::Linux);
    for (const ExportMenuAction &action : linuxActions) {
        assert(action.identifier != "copy_emf");
        assert(action.identifier != "copy_pdf");
    }

    const ExportCapabilities tableCapabilities =
        StandardTableExportCapabilities(ExportPlatform::MacOS, true);
    const auto tableActions = BuildExportMenuActions(tableCapabilities, ExportPlatform::MacOS);
    bool hasCsv = false;
    bool hasApa = false;
    bool hasDisplayedPdf = false;
    bool hasGenericPdf = false;
    std::set<std::string> tableActionIds;
    for (const ExportMenuAction &action : tableActions) {
        assert(tableActionIds.insert(action.identifier).second);
        hasCsv = hasCsv || action.capability == ExportCapability::SaveCsv;
        hasApa = hasApa || action.capability == ExportCapability::SaveApaPdf;
        hasDisplayedPdf = hasDisplayedPdf || action.capability == ExportCapability::SaveDisplayedPdf;
        hasGenericPdf = hasGenericPdf || action.capability == ExportCapability::SavePdf;
    }
    assert(hasCsv && hasApa && hasDisplayedPdf);
    assert(!hasGenericPdf);

    std::vector<ExportSurfaceRegistration> registry = {
        {"scatterplot", ExportSurfaceKind::Plot, mac},
        {"trellis", ExportSurfaceKind::Plot, mac}
    };
    assert(AuditExportSurfaceRegistry(registry, ExportPlatform::MacOS).empty());
    registry.push_back({"scatterplot", ExportSurfaceKind::Plot, mac});
    assert(!AuditExportSurfaceRegistry(registry, ExportPlatform::MacOS).empty());

    const auto declaredMacSurfaces = DefaultExportSurfaceRegistry(ExportPlatform::MacOS);
    assert(declaredMacSurfaces.size() >= 20);
    assert(AuditExportSurfaceRegistry(declaredMacSurfaces, ExportPlatform::MacOS).empty());
    bool hasCountModel = false;
    std::set<std::string> declaredIdentifiers;
    for (const auto &surface : declaredMacSurfaces) {
        declaredIdentifiers.insert(surface.identifier);
        if (surface.identifier == "count-model") {
            hasCountModel = true;
            assert(surface.kind == ExportSurfaceKind::Table);
            assert(surface.capabilities.has(ExportCapability::SaveCsv));
            assert(surface.capabilities.has(ExportCapability::SaveDisplayedPdf));
        }
    }
    assert(hasCountModel);
    for (const char *identifier : {
             "correlation-matrix", "dimensionality-report", "scale-analysis",
             "scale-analysis-derived", "interaction-interpretation",
             "missing-data-diagnostics", "multiple-imputation-diagnostics",
             "frequency-table"}) {
        assert(declaredIdentifiers.count(identifier) == 1);
    }
    const auto declaredWindowsSurfaces = DefaultExportSurfaceRegistry(ExportPlatform::Windows);
    assert(AuditExportSurfaceRegistry(declaredWindowsSurfaces, ExportPlatform::Windows).empty());
    ExportCapabilities incompleteWindowsTable =
        StandardTableExportCapabilities(ExportPlatform::Linux, true);
    assert(!AuditExportSurfaceRegistry({{"bad-table", ExportSurfaceKind::Table, incompleteWindowsTable}},
                                       ExportPlatform::Windows).empty());
    ExportCapabilities textOnlyTable;
    textOnlyTable.supported = {
        ExportCapability::CopyVectorNative, ExportCapability::SaveSvg,
        ExportCapability::CopyFormattedText
    };
    const auto textOnlyIssues = AuditExportSurfaceRegistry(
        {{"text-only-table", ExportSurfaceKind::Table, textOnlyTable}},
        ExportPlatform::Windows);
    assert(textOnlyIssues.size() >= 3);

    assert(SafeExportBaseName("  mtcars / scatterplot: mpg ~ wt  ") ==
           "mtcars-scatterplot-mpg-wt");
    assert(SafeExportBaseName("///", "plot") == "plot");
    assert(CsvReportToTabDelimited("Term,Warning\n\"a,b\",\"say \"\"hi\"\"\"\n") ==
           "Term\tWarning\na,b\tsay \"hi\"\n");
    return 0;
}
