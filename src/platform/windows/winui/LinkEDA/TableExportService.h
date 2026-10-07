#pragma once

#include <functional>
#include <memory>
#include <string>
#include <vector>

#include "../../../../core/provenance_model.h"

namespace winrt::LinkEDA::implementation
{
    struct TableExportCell
    {
        std::string text;
        bool alignRight = false;
        bool bold = false;
        bool muted = false;
        int indentLevel = 0;
    };

    struct TableExportRow
    {
        std::vector<TableExportCell> cells;
        bool separatorBefore = false;
    };

    struct TableExportPayload
    {
        std::string title;
        std::string subtitle;
        std::string baseName;
        std::vector<TableExportCell> headers;
        std::vector<TableExportRow> rows;
        std::string spanningHeader;
        std::size_t spanningColumnStart = 0;
        std::size_t spanningColumnCount = 0;
        std::vector<std::string> footnotes;
        std::vector<std::string> warnings;
        std::string csvText;
        std::string markdownText;
        // Shared semantic tables used by APA clipboard and LaTeX PDF export.
        // The displayed/native payload above remains available for other formats.
        std::vector<::rlispstat::core::PublicationTableSpec> publicationTables;
        // Optional alternate semantic layout selected in the shared APA dialog.
        std::vector<::rlispstat::core::PublicationTableSpec> alternatePublicationTables;
        // Optional standalone dimensionality tables. When selected, each table
        // is compiled to its own PDF and keeps dimensions in horizontal columns.
        std::vector<::rlispstat::core::PublicationTableSpec> separatePublicationTables;
    };

    // Shared by every native Windows result table. A table supplies an
    // immutable semantic payload; menu construction, clipboard formats, file
    // dialogs and styled vector/raster/PDF rendering live here once.
    class TableExportService final
        : public std::enable_shared_from_this<TableExportService>
    {
    public:
        using PayloadFactory = std::function<TableExportPayload()>;

        static std::shared_ptr<TableExportService> Create(
            Microsoft::UI::Xaml::Window const& owner,
            Microsoft::UI::Xaml::Controls::Grid const& visualHost);

        Microsoft::UI::Xaml::Controls::MenuFlyoutSubItem CreateMenu(
            PayloadFactory factory, bool supportsApaPdf = true);

    private:
        TableExportService(Microsoft::UI::Xaml::Window const& owner,
                           Microsoft::UI::Xaml::Controls::Grid const& visualHost);
        void Execute(std::string command, PayloadFactory const& factory);
        void CopyFormatted(TableExportPayload const& payload);
        void CopyTabDelimited(TableExportPayload const& payload);
        void SaveText(TableExportPayload const& payload, std::string const& format);
        winrt::fire_and_forget ExportApaAsync(TableExportPayload payload, bool copyToClipboard);
        winrt::fire_and_forget CopyImageAsync(TableExportPayload payload, bool svgClipboard);
        winrt::fire_and_forget SaveVisualAsync(TableExportPayload payload,
                                               std::string format);
        Windows::Foundation::IAsyncOperation<
            Windows::Storage::Streams::InMemoryRandomAccessStream>
            RenderSvgToPngAsync(std::string svg, double width, double height);
        Windows::Foundation::IAsyncOperation<bool>
            RenderSvgToPdfAsync(std::string svg, std::wstring path,
                                double width, double height, bool apaStyle);

        Microsoft::UI::Xaml::Window owner_{ nullptr };
        Microsoft::UI::Xaml::Controls::WebView2 renderer_{ nullptr };
    };
}
