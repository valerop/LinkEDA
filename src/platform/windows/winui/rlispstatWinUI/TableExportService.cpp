#include "pch.h"
#include "TableExportService.h"

#include "../../../../core/export_model.h"
#include "../../../../core/provenance_model.h"
#include "../../../../core/svg_writer.h"

#include <microsoft.ui.xaml.window.h>
#include <winrt/Windows.ApplicationModel.DataTransfer.h>

#include <algorithm>
#include <cmath>
#include <fstream>
#include <optional>
#include <shobjidl.h>
#include <sstream>
#include <vector>

using namespace winrt;
using namespace Microsoft::UI::Xaml;

namespace
{
    HWND WindowHandle(Window const& window)
    {
        HWND handle{};
        check_hresult(window.as<::IWindowNative>()->get_WindowHandle(&handle));
        return handle;
    }

    std::string HtmlEscape(std::string const& value)
    {
        std::string result;
        result.reserve(value.size() + 32);
        for (char ch : value)
        {
            switch (ch)
            {
            case '&': result += "&amp;"; break;
            case '<': result += "&lt;"; break;
            case '>': result += "&gt;"; break;
            case '"': result += "&quot;"; break;
            default: result.push_back(ch); break;
            }
        }
        return result;
    }

    using ExportPayload = winrt::rlispstatWinUI::implementation::TableExportPayload;
    using ExportCell = winrt::rlispstatWinUI::implementation::TableExportCell;

    std::string TableText(ExportPayload const& payload,
                          bool includeDocumentText,
                          bool includeNotes)
    {
        std::ostringstream out;
        if (includeDocumentText)
        {
            if (!payload.title.empty()) out << payload.title << "\n";
            if (!payload.subtitle.empty()) out << payload.subtitle << "\n";
            if (!payload.title.empty() || !payload.subtitle.empty()) out << "\n";
        }
        auto appendCells = [&out](auto const& cells)
        {
            for (std::size_t index = 0; index < cells.size(); ++index)
            {
                if (index > 0) out << '\t';
                for (int indent = 0; indent < cells[index].indentLevel; ++indent) out << "  ";
                out << cells[index].text;
            }
            out << '\n';
        };
        appendCells(payload.headers);
        for (auto const& row : payload.rows) appendCells(row.cells);
        if (includeNotes)
        {
            for (auto const& note : payload.footnotes) out << "\n" << note;
            for (auto const& warning : payload.warnings) out << "\nWarning: " << warning;
        }
        return out.str();
    }

    std::string DelimitedCell(std::string value, char delimiter)
    {
        const bool quote = value.find(delimiter) != std::string::npos ||
            value.find_first_of("\"\r\n") != std::string::npos;
        if (!quote) return value;
        std::string escaped;
        escaped.reserve(value.size() + 2);
        escaped.push_back('"');
        for (char ch : value) {
            if (ch == '"') escaped.push_back('"');
            escaped.push_back(ch);
        }
        escaped.push_back('"');
        return escaped;
    }

    std::string PayloadCsv(ExportPayload const& payload)
    {
        std::ostringstream out;
        auto append = [&out](auto const& cells) {
            for (std::size_t index = 0; index < cells.size(); ++index) {
                if (index) out << ',';
                out << DelimitedCell(cells[index].text, ',');
            }
            out << '\n';
        };
        append(payload.headers);
        for (auto const& row : payload.rows) append(row.cells);
        return out.str();
    }

    std::string MarkdownCell(std::string value)
    {
        std::string result;
        result.reserve(value.size());
        for (char ch : value) {
            if (ch == '|') result += "\\|";
            else if (ch == '\r' || ch == '\n') result.push_back(' ');
            else result.push_back(ch);
        }
        return result;
    }

    std::string PayloadMarkdown(ExportPayload const& payload)
    {
        std::ostringstream out;
        if (!payload.title.empty()) out << "### " << payload.title << "\n\n";
        if (!payload.subtitle.empty()) out << payload.subtitle << "\n\n";
        out << '|';
        for (auto const& cell : payload.headers)
            out << ' ' << MarkdownCell(cell.text) << " |";
        out << "\n|";
        for (auto const& cell : payload.headers)
            out << (cell.alignRight ? " ---: |" : " :--- |");
        out << '\n';
        for (auto const& row : payload.rows) {
            out << '|';
            for (std::size_t index = 0; index < payload.headers.size(); ++index) {
                const std::string value = index < row.cells.size()
                    ? row.cells[index].text : std::string{};
                out << ' ' << MarkdownCell(value) << " |";
            }
            out << '\n';
        }
        for (auto const& note : payload.footnotes) out << "\n" << note << "\n";
        for (auto const& warning : payload.warnings)
            out << "\n**Warning:** " << warning << "\n";
        return out.str();
    }

    std::string CellStyle(ExportCell const& cell, bool header, bool separator)
    {
        std::ostringstream style;
        style << "padding:6px 12px;vertical-align:top;text-align:"
              << (cell.alignRight ? "right" : "left") << ";";
        if (cell.indentLevel > 0)
            style << "padding-left:" << (12 + 22 * cell.indentLevel) << "px;";
        if (header || cell.bold) style << "font-weight:600;";
        if (cell.muted) style << "color:#6f6f6f;";
        if (separator) style << "border-top:1px solid #d3d3d3;";
        return style.str();
    }

    std::string TableHtml(ExportPayload const& payload)
    {
        std::string html =
            "<div style=\"font-family:Arial,sans-serif;color:#222\"><h2 style=\"margin:0 0 8px\">" +
            HtmlEscape(payload.title) + "</h2>";
        if (!payload.subtitle.empty())
            html += "<div style=\"color:#666;margin-bottom:18px\">" +
                HtmlEscape(payload.subtitle) + "</div>";
        html += "<table style=\"border-collapse:collapse;min-width:520px\">";
        if (!payload.spanningHeader.empty() && payload.spanningColumnCount > 0)
        {
            html += "<tr>";
            if (payload.spanningColumnStart > 0)
                html += "<th colspan=\"" + std::to_string(payload.spanningColumnStart) + "\"></th>";
            html += "<th colspan=\"" + std::to_string(payload.spanningColumnCount) +
                "\" style=\"padding:4px 12px;text-align:center;font-weight:600\">" +
                HtmlEscape(payload.spanningHeader) + "</th>";
            const std::size_t consumed = payload.spanningColumnStart + payload.spanningColumnCount;
            if (consumed < payload.headers.size())
                html += "<th colspan=\"" + std::to_string(payload.headers.size() - consumed) + "\"></th>";
            html += "</tr>";
        }
        html += "<thead><tr style=\"border-top:1px solid #888;border-bottom:1px solid #888\">";
        for (auto const& cell : payload.headers)
            html += "<th style=\"" + CellStyle(cell, true, false) + "\">" +
                HtmlEscape(cell.text) + "</th>";
        html += "</tr></thead><tbody>";
        for (auto const& row : payload.rows)
        {
            html += "<tr>";
            for (auto const& cell : row.cells)
            {
                html += "<td style=\"" + CellStyle(cell, false, row.separatorBefore) + "\">" +
                    HtmlEscape(cell.text) + "</td>";
            }
            html += "</tr>";
        }
        html += "</tbody></table>";
        if (!payload.footnotes.empty() || !payload.warnings.empty())
            html += "<div style=\"border-top:1px solid #aaa;margin-top:8px;padding-top:8px;font-size:11px;color:#666\">";
        for (auto const& note : payload.footnotes)
            html += "<div>" + HtmlEscape(note) + "</div>";
        for (auto const& warning : payload.warnings)
            html += "<div style=\"color:#8c2e24\">Warning: " + HtmlEscape(warning) + "</div>";
        if (!payload.footnotes.empty() || !payload.warnings.empty()) html += "</div>";
        return html + "</div>";
    }

    std::vector<std::string> WrapText(std::string const& value, std::size_t maximumCharacters)
    {
        std::vector<std::string> lines;
        std::istringstream words(value);
        std::string word;
        std::string line;
        while (words >> word)
        {
            if (!line.empty() && line.size() + 1 + word.size() > maximumCharacters)
            {
                lines.push_back(line);
                line.clear();
            }
            if (!line.empty()) line += ' ';
            line += word;
        }
        if (!line.empty()) lines.push_back(line);
        if (lines.empty()) lines.push_back(std::string{});
        return lines;
    }

    ::rlispstat::core::SvgTableDocument StyledTableSvg(
        ExportPayload const& payload, bool apaStyle)
    {
        const auto layout = ::rlispstat::core::BuildVectorTableLayout(
            std::string{}, TableText(payload, false, false), 1200.0);
        const double width = std::max(520.0, layout.dimensions.widthPoints);
        const double margin = 24.0;
        const double rowHeight = 23.0;
        const double headerHeight = 26.0;
        const double spanningHeight = payload.spanningHeader.empty() ? 0.0 : 22.0;
        const double titleHeight = apaStyle ? 58.0 : 48.0;
        const std::size_t maximumCharacters = static_cast<std::size_t>(
            std::max(36.0, std::floor((width - 2.0 * margin) / 5.8)));
        std::vector<std::pair<std::string, bool>> noteLines;
        for (auto const& note : payload.footnotes)
            for (auto const& line : WrapText(note, maximumCharacters))
                noteLines.emplace_back(line, false);
        for (auto const& warning : payload.warnings)
            for (auto const& line : WrapText("Warning: " + warning, maximumCharacters))
                noteLines.emplace_back(line, true);
        const double notesHeight = noteLines.empty() ? 0.0 :
            17.0 + 15.0 * static_cast<double>(noteLines.size());
        const double tableTop = margin + titleHeight + spanningHeight;
        const double tableBottom = tableTop + headerHeight +
            rowHeight * static_cast<double>(payload.rows.size());
        const double height = tableBottom + notesHeight + margin;

        ::rlispstat::core::SvgTableDocument document;
        document.dimensions = {width, height, 2.0};
        ::rlispstat::core::SvgWriter writer(document.dimensions);
        writer.rectangle(0.0, 0.0, width, height,
                         {"#ffffff", "none", 0.0, 1.0, {}});

        if (apaStyle)
        {
            writer.text(margin, margin + 14.0, "Table 1",
                        {"#222222", "Arial, Helvetica, sans-serif", 12.0, 600, false, "start"});
            writer.text(margin, margin + 34.0,
                        ::rlispstat::core::PublicationTitleWithoutTableLabel(payload.title),
                        {"#222222", "Arial, Helvetica, sans-serif", 12.0, 400, true, "start"});
        }
        else
        {
            writer.text(margin, margin + 17.0, payload.title,
                        {"#222222", "Arial, Helvetica, sans-serif", 16.0, 600, false, "start"});
        }
        if (!payload.subtitle.empty())
            writer.text(margin, margin + (apaStyle ? 52.0 : 38.0), payload.subtitle,
                        {"#666666", "Arial, Helvetica, sans-serif", 10.5, 400, false, "start"});

        if (!payload.spanningHeader.empty() && payload.spanningColumnCount > 0 &&
            payload.spanningColumnStart < layout.columnStarts.size())
        {
            const std::size_t end = std::min(layout.columnWidths.size(),
                payload.spanningColumnStart + payload.spanningColumnCount);
            double spanWidth = 0.0;
            for (std::size_t index = payload.spanningColumnStart; index < end; ++index)
                spanWidth += layout.columnWidths[index];
            writer.text(layout.columnStarts[payload.spanningColumnStart] + spanWidth / 2.0,
                        tableTop - 7.0, payload.spanningHeader,
                        {"#333333", "Arial, Helvetica, sans-serif", 11.0, 600, false, "middle"});
        }

        ::rlispstat::core::SvgPaint rule{"none", "#8a8a8a", 0.8, 1.0, {}};
        ::rlispstat::core::SvgPaint lightRule{"none", "#d3d3d3", 0.65, 1.0, {}};
        writer.line(margin, tableTop, width - margin, tableTop, rule);
        if (!apaStyle)
            writer.rectangle(margin, tableTop, width - 2.0 * margin, headerHeight,
                             {"#f7f7f7", "none", 0.0, 1.0, {}});
        for (std::size_t column = 0; column < layout.columnWidths.size(); ++column)
        {
            const ExportCell cell = column < payload.headers.size()
                ? payload.headers[column] : ExportCell{};
            const double x = layout.columnStarts[column];
            const double cellWidth = layout.columnWidths[column];
            writer.beginClipRect(x + 1.0, tableTop + 1.0,
                                 std::max(0.0, cellWidth - 2.0), headerHeight - 2.0);
            const double textX = cell.alignRight ? x + cellWidth - 6.0 : x + 6.0;
            writer.text(textX, tableTop + 17.0, cell.text,
                        {"#222222", "Arial, Helvetica, sans-serif", 11.0, 600,
                         false, cell.alignRight ? "end" : "start"});
            writer.endClip();
        }
        writer.line(margin, tableTop + headerHeight, width - margin,
                    tableTop + headerHeight, rule);

        for (std::size_t rowIndex = 0; rowIndex < payload.rows.size(); ++rowIndex)
        {
            auto const& row = payload.rows[rowIndex];
            const double y = tableTop + headerHeight + rowHeight * static_cast<double>(rowIndex);
            if (row.separatorBefore) writer.line(margin, y, width - margin, y, lightRule);
            for (std::size_t column = 0; column < layout.columnWidths.size(); ++column)
            {
                const ExportCell cell = column < row.cells.size() ? row.cells[column] : ExportCell{};
                const double x = layout.columnStarts[column];
                const double cellWidth = layout.columnWidths[column];
                writer.beginClipRect(x + 1.0, y + 1.0,
                                     std::max(0.0, cellWidth - 2.0), rowHeight - 2.0);
                const double leftPadding = 6.0 + 20.0 * static_cast<double>(cell.indentLevel);
                const double textX = cell.alignRight ? x + cellWidth - 6.0 : x + leftPadding;
                writer.text(textX, y + 16.0, cell.text,
                            {cell.muted ? "#707070" : "#222222",
                             "Arial, Helvetica, sans-serif", 11.0,
                             cell.bold ? 600 : 400, false,
                             cell.alignRight ? "end" : "start"});
                writer.endClip();
            }
        }
        writer.line(margin, tableBottom, width - margin, tableBottom, rule);
        if (!noteLines.empty())
        {
            double y = tableBottom + 18.0;
            for (auto const& [line, warning] : noteLines)
            {
                writer.text(margin, y, line,
                            {warning ? "#8c2e24" : "#666666",
                             "Arial, Helvetica, sans-serif", 9.5, 400, false, "start"});
                y += 15.0;
            }
        }
        document.svg = writer.finish();
        return document;
    }

    std::optional<std::wstring> ChoosePath(Window const& owner,
                                            std::string const& baseName,
                                            std::string const& format)
    {
        com_ptr<IFileSaveDialog> dialog;
        if (FAILED(CoCreateInstance(CLSID_FileSaveDialog, nullptr, CLSCTX_INPROC_SERVER,
                                    IID_PPV_ARGS(dialog.put())))) return std::nullopt;
        const std::wstring extension = to_hstring(format == "markdown" ? "md" : format).c_str();
        const std::wstring pattern = L"*." + extension;
        const std::wstring label = to_hstring(format + " files").c_str();
        const COMDLG_FILTERSPEC filter{label.c_str(), pattern.c_str()};
        dialog->SetFileTypes(1, &filter);
        dialog->SetDefaultExtension(extension.c_str());
        dialog->SetFileName(to_hstring(
            ::rlispstat::core::SafeExportBaseName(baseName, "LinkEDA-table") + "." +
            to_string(hstring(extension))).c_str());
        dialog->SetTitle(L"Export Table");
        FILEOPENDIALOGOPTIONS options{};
        dialog->GetOptions(&options);
        dialog->SetOptions(options | FOS_FORCEFILESYSTEM | FOS_OVERWRITEPROMPT |
                           FOS_STRICTFILETYPES);
        const HRESULT shown = dialog->Show(WindowHandle(owner));
        if (shown == HRESULT_FROM_WIN32(ERROR_CANCELLED) || FAILED(shown)) return std::nullopt;
        com_ptr<IShellItem> selected;
        if (FAILED(dialog->GetResult(selected.put()))) return std::nullopt;
        PWSTR rawPath{};
        if (FAILED(selected->GetDisplayName(SIGDN_FILESYSPATH, &rawPath)) || !rawPath)
            return std::nullopt;
        std::wstring path(rawPath);
        CoTaskMemFree(rawPath);
        return path;
    }

    bool WriteText(std::wstring const& path, std::string const& text)
    {
        std::ofstream output(path, std::ios::binary | std::ios::trunc);
        output.write(text.data(), static_cast<std::streamsize>(text.size()));
        return output.good();
    }

    void Report(Window const& owner, wchar_t const* message, bool error)
    {
        MessageBoxW(WindowHandle(owner), message, L"Export Table",
                    MB_OK | (error ? MB_ICONERROR : MB_ICONINFORMATION));
    }
}

namespace winrt::rlispstatWinUI::implementation
{
    std::shared_ptr<TableExportService> TableExportService::Create(
        Window const& owner, Controls::Grid const& visualHost)
    {
        return std::shared_ptr<TableExportService>(
            new TableExportService(owner, visualHost));
    }

    TableExportService::TableExportService(
        Window const& owner, Controls::Grid const& visualHost) : owner_(owner)
    {
        renderer_ = Controls::WebView2();
        renderer_.Opacity(0.001);
        renderer_.Visibility(Visibility::Collapsed);
        renderer_.IsHitTestVisible(false);
        renderer_.HorizontalAlignment(HorizontalAlignment::Left);
        renderer_.VerticalAlignment(VerticalAlignment::Top);
        visualHost.Children().InsertAt(0, renderer_);
    }

    Controls::MenuFlyoutSubItem TableExportService::CreateMenu(
        PayloadFactory factory, bool supportsApaPdf)
    {
        auto submenu = Controls::MenuFlyoutSubItem();
        submenu.Text(L"Export");
        const auto capabilities = ::rlispstat::core::StandardTableExportCapabilities(
            ::rlispstat::core::ExportPlatform::Windows, supportsApaPdf);
        const auto actions = ::rlispstat::core::BuildExportMenuActions(
            capabilities, ::rlispstat::core::ExportPlatform::Windows);
        auto copyMenu = Controls::MenuFlyoutSubItem();
        copyMenu.Text(L"Copy");
        auto saveMenu = Controls::MenuFlyoutSubItem();
        saveMenu.Text(L"Save");
        bool markdownAdded = false;
        for (auto const& action : actions)
        {
            auto destination = action.command.rfind("COPY_", 0) == 0
                ? copyMenu : saveMenu;
            if (action.separatorBefore && destination.Items().Size() > 0)
                destination.Items().Append(Controls::MenuFlyoutSeparator());
            auto item = Controls::MenuFlyoutItem();
            item.Text(to_hstring(action.title));
            item.Click([self = shared_from_this(), command = action.command, factory]
                (auto const&, auto const&) { self->Execute(command, factory); });
            destination.Items().Append(item);
            if (action.command == "SAVE_CSV")
            {
                auto markdown = Controls::MenuFlyoutItem();
                markdown.Text(L"Markdown...");
                markdown.Click([self = shared_from_this(), factory](auto const&, auto const&)
                    { self->Execute("SAVE_MARKDOWN", factory); });
                saveMenu.Items().Append(markdown);
                markdownAdded = true;
            }
        }
        if (!markdownAdded)
        {
            auto markdown = Controls::MenuFlyoutItem();
            markdown.Text(L"Markdown...");
            markdown.Click([self = shared_from_this(), factory](auto const&, auto const&)
                { self->Execute("SAVE_MARKDOWN", factory); });
            saveMenu.Items().Append(markdown);
        }
        if (copyMenu.Items().Size() > 0) submenu.Items().Append(copyMenu);
        if (saveMenu.Items().Size() > 0) submenu.Items().Append(saveMenu);
        return submenu;
    }

    void TableExportService::Execute(std::string command, PayloadFactory const& factory)
    {
        const TableExportPayload payload = factory();
        if (command == "COPY_FORMATTED_TEXT" || command == "COPY_RICH")
            CopyFormatted(payload);
        else if (command == "COPY_TAB_DELIMITED_TEXT") CopyTabDelimited(payload);
        else if (command == "COPY_SVG") CopyImageAsync(payload, true);
        else if (command == "COPY_PNG") CopyImageAsync(payload, false);
        else if (command == "SAVE_CSV") SaveText(payload, "csv");
        else if (command == "SAVE_MARKDOWN") SaveText(payload, "markdown");
        else if (command == "SAVE_SVG" || command == "SAVE_PDF" || command == "SAVE_PNG" ||
                 command == "SAVE_DISPLAYED_PDF" || command == "SAVE_APA_PDF")
            SaveVisualAsync(payload,
                command == "SAVE_SVG" ? "svg" :
                (command == "SAVE_PNG" ? "png" : "pdf"),
                command == "SAVE_APA_PDF");
    }

    void TableExportService::CopyFormatted(TableExportPayload const& payload)
    {
        Windows::ApplicationModel::DataTransfer::DataPackage package;
        package.SetText(to_hstring(TableText(payload, true, true)));
        package.SetHtmlFormat(Windows::ApplicationModel::DataTransfer::HtmlFormatHelper::
            CreateHtmlFormat(to_hstring(TableHtml(payload))));
        Windows::ApplicationModel::DataTransfer::Clipboard::SetContent(package);
        Windows::ApplicationModel::DataTransfer::Clipboard::Flush();
    }

    void TableExportService::CopyTabDelimited(TableExportPayload const& payload)
    {
        Windows::ApplicationModel::DataTransfer::DataPackage package;
        package.SetText(to_hstring(TableText(payload, false, false)));
        Windows::ApplicationModel::DataTransfer::Clipboard::SetContent(package);
        Windows::ApplicationModel::DataTransfer::Clipboard::Flush();
    }

    void TableExportService::SaveText(TableExportPayload const& payload,
                                      std::string const& format)
    {
        const auto path = ChoosePath(owner_, payload.baseName, format);
        if (!path) return;
        const std::string value = format == "csv"
            ? (payload.csvText.empty() ? PayloadCsv(payload) : payload.csvText)
            : (payload.markdownText.empty() ? PayloadMarkdown(payload) : payload.markdownText);
        if (!WriteText(*path, value)) Report(owner_, L"The table could not be exported.", true);
    }

    Windows::Foundation::IAsyncOperation<Windows::Storage::Streams::InMemoryRandomAccessStream>
        TableExportService::RenderSvgToPngAsync(
            std::string svg, double width, double height)
    {
        renderer_.Width(width * 2.0); renderer_.Height(height * 2.0);
        renderer_.Visibility(Visibility::Visible);
        co_await renderer_.EnsureCoreWebView2Async();
        const std::string html =
            "<!doctype html><html><head><meta charset=\"utf-8\"><style>"
            "html,body{margin:0;background:white;overflow:hidden}svg{width:100vw;height:100vh}"
            "</style></head><body>" + svg + "</body></html>";
        winrt::handle completed(CreateEventW(nullptr, TRUE, FALSE, nullptr));
        event_token token = renderer_.NavigationCompleted(
            [event = completed.get()](auto const&, auto const&) { SetEvent(event); });
        renderer_.NavigateToString(to_hstring(html));
        co_await winrt::resume_on_signal(completed.get());
        co_await wil::resume_foreground(renderer_.DispatcherQueue());
        renderer_.NavigationCompleted(token);
        Windows::Storage::Streams::InMemoryRandomAccessStream stream;
        co_await renderer_.CoreWebView2().CapturePreviewAsync(
            Microsoft::Web::WebView2::Core::CoreWebView2CapturePreviewImageFormat::Png, stream);
        renderer_.Visibility(Visibility::Collapsed);
        stream.Seek(0);
        co_return stream;
    }

    Windows::Foundation::IAsyncOperation<bool> TableExportService::RenderSvgToPdfAsync(
        std::string svg, std::wstring path, double width, double height, bool apaStyle)
    {
        (void)apaStyle;
        renderer_.Width(width); renderer_.Height(height);
        renderer_.Visibility(Visibility::Visible);
        co_await renderer_.EnsureCoreWebView2Async();
        const std::string html =
            "<!doctype html><html><head><meta charset=\"utf-8\"><style>"
            "@page{margin:0;size:" + std::to_string(width) + "px " +
            std::to_string(height) + "px}html,body{margin:0;width:" +
            std::to_string(width) + "px;height:" + std::to_string(height) +
            "px;background:white;overflow:hidden}svg{display:block;width:" +
            std::to_string(width) + "px;height:" + std::to_string(height) + "px}"
            "</style></head><body>" + svg + "</body></html>";
        winrt::handle completed(CreateEventW(nullptr, TRUE, FALSE, nullptr));
        event_token token = renderer_.NavigationCompleted(
            [event = completed.get()](auto const&, auto const&) { SetEvent(event); });
        renderer_.NavigateToString(to_hstring(html));
        co_await winrt::resume_on_signal(completed.get());
        co_await wil::resume_foreground(renderer_.DispatcherQueue());
        renderer_.NavigationCompleted(token);
        auto settings = renderer_.CoreWebView2().Environment().CreatePrintSettings();
        settings.ShouldPrintBackgrounds(true); settings.ShouldPrintHeaderAndFooter(false);
        settings.MarginTop(0); settings.MarginRight(0); settings.MarginBottom(0); settings.MarginLeft(0);
        settings.PageWidth(width / 96.0); settings.PageHeight(height / 96.0);
        const bool ok = co_await renderer_.CoreWebView2().PrintToPdfAsync(path, settings);
        renderer_.Visibility(Visibility::Collapsed);
        co_return ok;
    }

    fire_and_forget TableExportService::CopyImageAsync(
        TableExportPayload payload, bool svgClipboard)
    {
        auto lifetime = shared_from_this();
        try
        {
            const auto document = StyledTableSvg(payload, false);
            auto stream = co_await RenderSvgToPngAsync(document.svg,
                document.dimensions.widthPoints, document.dimensions.heightPoints);
            Windows::ApplicationModel::DataTransfer::DataPackage package;
            package.SetBitmap(Windows::Storage::Streams::RandomAccessStreamReference::
                CreateFromStream(stream));
            if (svgClipboard) package.SetData(L"image/svg+xml", box_value(to_hstring(document.svg)));
            Windows::ApplicationModel::DataTransfer::Clipboard::SetContent(package);
            Windows::ApplicationModel::DataTransfer::Clipboard::Flush();
        }
        catch (...) { renderer_.Visibility(Visibility::Collapsed); Report(owner_, L"The table could not be copied.", true); }
    }

    fire_and_forget TableExportService::SaveVisualAsync(
        TableExportPayload payload, std::string format, bool apaStyle)
    {
        auto lifetime = shared_from_this();
        const auto path = ChoosePath(owner_, payload.baseName, format);
        if (!path) co_return;
        bool ok = false;
        try
        {
            const auto document = StyledTableSvg(payload, apaStyle);
            if (format == "svg") ok = WriteText(*path, document.svg);
            else if (format == "pdf") ok = co_await RenderSvgToPdfAsync(document.svg, *path,
                document.dimensions.widthPoints, document.dimensions.heightPoints, apaStyle);
            else
            {
                auto stream = co_await RenderSvgToPngAsync(document.svg,
                    document.dimensions.widthPoints, document.dimensions.heightPoints);
                const uint32_t length = static_cast<uint32_t>(stream.Size());
                Windows::Storage::Streams::DataReader reader(stream.GetInputStreamAt(0));
                co_await reader.LoadAsync(length);
                std::vector<uint8_t> bytes(length); reader.ReadBytes(bytes);
                std::ofstream output(*path, std::ios::binary | std::ios::trunc);
                output.write(reinterpret_cast<char const*>(bytes.data()),
                    static_cast<std::streamsize>(bytes.size()));
                ok = output.good();
            }
        }
        catch (...) { renderer_.Visibility(Visibility::Collapsed); ok = false; }
        if (!ok) Report(owner_, L"The table could not be exported.", true);
        owner_.Activate();
    }
}
