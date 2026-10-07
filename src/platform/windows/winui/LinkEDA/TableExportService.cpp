#include "pch.h"
#include "TableExportService.h"

#include "../../../../core/export_model.h"
#include "../../../../core/provenance_model.h"
#include "../../../../core/svg_writer.h"

#include <microsoft.ui.xaml.window.h>
#include <winrt/Windows.ApplicationModel.DataTransfer.h>

#include <algorithm>
#include <cctype>
#include <cmath>
#include <filesystem>
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

    using ExportPayload = winrt::LinkEDA::implementation::TableExportPayload;
    using ExportCell = winrt::LinkEDA::implementation::TableExportCell;

    bool IsTypeColumn(std::string value)
    {
        std::transform(value.begin(), value.end(), value.begin(),
            [](unsigned char character) { return static_cast<char>(std::tolower(character)); });
        value.erase(value.begin(), std::find_if(value.begin(), value.end(),
            [](unsigned char character) { return !std::isspace(character); }));
        value.erase(std::find_if(value.rbegin(), value.rend(),
            [](unsigned char character) { return !std::isspace(character); }).base(), value.end());
        return value == "type" || value == "variable type" || value == "predictor type";
    }

    bool IsStandardizedBetaColumn(std::string value)
    {
        value.erase(value.begin(), std::find_if(value.begin(), value.end(),
            [](unsigned char character) { return !std::isspace(character); }));
        value.erase(std::find_if(value.rbegin(), value.rend(),
            [](unsigned char character) { return !std::isspace(character); }).base(), value.end());
        std::string lower = value;
        std::transform(lower.begin(), lower.end(), lower.begin(),
            [](unsigned char character) { return static_cast<char>(std::tolower(character)); });
        return value == "\xCE\xB2" || lower == "beta" || lower == "standardized beta";
    }

    bool IsEmptyPublicationCell(std::string value)
    {
        value.erase(value.begin(), std::find_if(value.begin(), value.end(),
            [](unsigned char character) { return !std::isspace(character); }));
        value.erase(std::find_if(value.rbegin(), value.rend(),
            [](unsigned char character) { return !std::isspace(character); }).base(), value.end());
        return value.empty() || value == "-" || value == "\xE2\x80\x94";
    }

    void RemovePayloadColumn(ExportPayload& payload, std::size_t column)
    {
        payload.headers.erase(payload.headers.begin() + column);
        for (auto& row : payload.rows)
            if (column < row.cells.size()) row.cells.erase(row.cells.begin() + column);
        if (column < payload.spanningColumnStart)
            --payload.spanningColumnStart;
        else if (column < payload.spanningColumnStart + payload.spanningColumnCount &&
                 payload.spanningColumnCount > 0)
            --payload.spanningColumnCount;
    }

    ExportPayload ApaPayload(ExportPayload payload)
    {
        // Type is useful while editing a model, but it is implementation
        // metadata rather than a reported APA result. Remove it here so every
        // current and future model follows the same export rule.
        for (std::size_t column = payload.headers.size(); column-- > 0;)
        {
            if (!IsTypeColumn(payload.headers[column].text)) continue;
            RemovePayloadColumn(payload, column);
        }
        // β is defined by LinkEDA only for ordinary numeric main effects. A
        // factors-only model therefore has no standardized coefficients; do
        // not export a misleading column made entirely of em dashes.
        for (std::size_t column = payload.headers.size(); column-- > 0;)
        {
            if (!IsStandardizedBetaColumn(payload.headers[column].text)) continue;
            const bool empty = std::all_of(payload.rows.begin(), payload.rows.end(),
                [column](auto const& row)
                {
                    return column >= row.cells.size() ||
                        IsEmptyPublicationCell(row.cells[column].text);
                });
            if (empty) RemovePayloadColumn(payload, column);
        }
        return payload;
    }

    std::string PublicationReportText(
        std::vector<::rlispstat::core::PublicationTableSpec> const& tables,
        std::string const& number, std::string const& title)
    {
        std::string plain = "Table " + number + "\n" + title + "\n";
        for (auto const& table : tables)
        {
            plain += "\n" + table.title + "\n";
            plain += ::rlispstat::core::BuildPublicationTableTsv(table);
            if (!table.footnotes.empty())
            {
                plain += "\nNote. ";
                for (std::size_t index = 0; index < table.footnotes.size(); ++index)
                {
                    if (index) plain += " ";
                    plain += table.footnotes[index];
                }
            }
            plain += "\n";
        }
        return plain;
    }

    std::string PublicationReportHtml(
        std::vector<::rlispstat::core::PublicationTableSpec> const& tables,
        std::string const& number, std::string const& title)
    {
        std::string html = "<html><head><meta charset=\"utf-8\"></head><body>";
        for (std::size_t index = 0; index < tables.size(); ++index)
        {
            const auto fragment = ::rlispstat::core::BuildPublicationTableHtml(
                tables[index], index == 0, number, title);
            const auto begin = fragment.find("<body>");
            const auto end = fragment.rfind("</body>");
            html += begin == std::string::npos || end == std::string::npos
                ? fragment : fragment.substr(begin + 6, end - begin - 6);
        }
        return html + "</body></html>";
    }

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

    std::string CellStyle(ExportCell const& cell, bool header, bool separator,
                          bool bottomRule = false, bool apaStyle = false)
    {
        std::ostringstream style;
        // Keep the rich clipboard table compact enough for Word's printable
        // area.  Point units are interpreted consistently by Office, whereas
        // the previous pixel padding made every additional column noticeably
        // wider after pasting.
        style << (apaStyle ? "padding:1pt 3pt;" : "padding:2.5pt 4pt;")
              << "vertical-align:top;white-space:normal;"
                 "word-wrap:break-word;text-align:"
              << (cell.alignRight ? "right" : "left") << ";";
        if (cell.indentLevel > 0)
            style << "padding-left:" << (4 + 10 * cell.indentLevel) << "pt;";
        if (header || cell.bold) style << "font-weight:600;";
        if (cell.muted) style << "color:#6f6f6f;";
        if (separator) style << "border-top:1px solid #d3d3d3;";
        if (bottomRule) style << "border-bottom:1px solid #222;";
        return style.str();
    }

    bool ColumnBodyIsRightAligned(ExportPayload const& payload, std::size_t column)
    {
        bool hasValue = false;
        for (auto const& row : payload.rows)
        {
            if (column >= row.cells.size() || row.cells[column].text.empty()) continue;
            hasValue = true;
            if (!row.cells[column].alignRight) return false;
        }
        return hasValue;
    }

    std::string TableHtml(ExportPayload const& payload, bool apaStyle,
                          std::string const& tableNumber = "1",
                          std::string const& apaTitle = {})
    {
        const std::string publicationTitle = apaTitle.empty()
            ? ::rlispstat::core::PublicationTitleWithoutTableLabel(payload.title)
            : apaTitle;
        std::string html = apaStyle
            ? "<div style=\"font-family:'Times New Roman',serif;font-size:10pt;"
              "line-height:1.15;color:#111;max-width:6.25in\"><p style=\"font-size:12pt;"
              "font-weight:bold;margin:0\">Table " + HtmlEscape(tableNumber) +
              "</p><p style=\"font-size:12pt;"
              "font-style:italic;margin:0 0 6pt\">" +
              HtmlEscape(publicationTitle) + "</p>"
            : "<div style=\"font-family:Arial,sans-serif;font-size:9pt;line-height:1.15;"
              "color:#222;max-width:6.25in\"><h2 style=\"font-size:12pt;line-height:1.15;"
              "margin:0 0 4pt\">" + HtmlEscape(payload.title) + "</h2>";
        if (!payload.subtitle.empty())
            html += std::string("<div style=\"") +
                (apaStyle ? "color:#222;" : "color:#666;") + "margin-bottom:6pt\">" +
                HtmlEscape(payload.subtitle) + "</div>";
        // The explicit percentage width is important for Word: it requests
        // AutoFit-to-window on paste instead of preserving the screen width.
        html += "<table width=\"100%\" style=\"width:100%;max-width:6.25in;"
                "border-collapse:collapse;table-layout:fixed;font-family:" +
                std::string(apaStyle ? "'Times New Roman',serif;" : "Arial,sans-serif;") +
                "font-size:9pt;line-height:" + std::string(apaStyle ? "1.0" : "1.15") + "\">";
        if (!payload.spanningHeader.empty() && payload.spanningColumnCount > 0)
        {
            html += "<tr>";
            if (payload.spanningColumnStart > 0)
                html += "<th colspan=\"" + std::to_string(payload.spanningColumnStart) + "\"></th>";
            html += "<th colspan=\"" + std::to_string(payload.spanningColumnCount) +
                "\" style=\"padding:2.5pt 4pt;text-align:center;font-weight:600\">" +
                HtmlEscape(payload.spanningHeader) + "</th>";
            const std::size_t consumed = payload.spanningColumnStart + payload.spanningColumnCount;
            if (consumed < payload.headers.size())
                html += "<th colspan=\"" + std::to_string(payload.headers.size() - consumed) + "\"></th>";
            html += "</tr>";
        }
        html += "<thead><tr style=\"border-top:1px solid #888;border-bottom:1px solid #888\">";
        for (std::size_t column = 0; column < payload.headers.size(); ++column)
        {
            auto const& cell = payload.headers[column];
            auto style = CellStyle(cell, true, false, false, apaStyle);
            if (ColumnBodyIsRightAligned(payload, column)) style += "text-align:center;";
            html += "<th style=\"" + style + "\">" + HtmlEscape(cell.text) + "</th>";
        }
        html += "</tr></thead><tbody>";
        for (std::size_t rowIndex = 0; rowIndex < payload.rows.size(); ++rowIndex)
        {
            auto const& row = payload.rows[rowIndex];
            html += "<tr>";
            for (auto const& cell : row.cells)
            {
                html += "<td style=\"" + CellStyle(cell, false,
                    !apaStyle && row.separatorBefore,
                    apaStyle && rowIndex + 1 == payload.rows.size(), apaStyle) + "\">" +
                    HtmlEscape(cell.text) + "</td>";
            }
            html += "</tr>";
        }
        html += "</tbody></table>";
        if (!payload.footnotes.empty() || !payload.warnings.empty())
            html += "<div style=\"border-top:1px solid #aaa;margin-top:4pt;"
                    "padding-top:4pt;font-size:8pt;line-height:1.15;color:#666\">";
        for (std::size_t index = 0; index < payload.footnotes.size(); ++index)
            html += "<div>" + std::string(apaStyle && index == 0 ? "<i>Note.</i> " : "") +
                HtmlEscape(payload.footnotes[index]) + "</div>";
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
        ExportPayload const& payload, bool apaStyle,
        std::string const& tableNumber = "1",
        std::string const& apaTitle = {})
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
            writer.text(margin, margin + 14.0, "Table " + tableNumber,
                        {"#222222", "Arial, Helvetica, sans-serif", 12.0, 600, false, "start"});
            writer.text(margin, margin + 34.0,
                        apaTitle.empty()
                            ? ::rlispstat::core::PublicationTitleWithoutTableLabel(payload.title)
                            : apaTitle,
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
            const bool centered = ColumnBodyIsRightAligned(payload, column);
            const double textX = centered ? x + cellWidth / 2.0 :
                (cell.alignRight ? x + cellWidth - 6.0 : x + 6.0);
            writer.text(textX, tableTop + 17.0, cell.text,
                        {"#222222", "Arial, Helvetica, sans-serif", 11.0, 600,
                         false, centered ? "middle" : (cell.alignRight ? "end" : "start")});
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

    std::filesystem::path PublicationRscriptPath()
    {
        auto fromRoot = [](std::filesystem::path root) -> std::filesystem::path {
            for (auto const& relative : {L"bin\\x64\\Rscript.exe", L"bin\\Rscript.exe"}) {
                const auto candidate = root / relative;
                if (std::filesystem::exists(candidate)) return candidate;
            }
            return {};
        };
        wchar_t value[32768]{};
        const auto length = GetEnvironmentVariableW(L"R_HOME", value,
            static_cast<DWORD>(std::size(value)));
        if (length > 0 && length < std::size(value))
            if (auto path = fromRoot(value); !path.empty()) return path;
        for (HKEY root : {HKEY_CURRENT_USER, HKEY_LOCAL_MACHINE}) {
            DWORD bytes = sizeof(value);
            if (RegGetValueW(root, L"SOFTWARE\\R-core\\R", L"InstallPath",
                    RRF_RT_REG_SZ, nullptr, value, &bytes) == ERROR_SUCCESS)
                if (auto path = fromRoot(value); !path.empty()) return path;
        }
        const std::filesystem::path installations = L"C:\\Program Files\\R";
        std::vector<std::filesystem::path> candidates;
        std::error_code error;
        if (std::filesystem::is_directory(installations, error))
            for (auto const& entry : std::filesystem::directory_iterator(installations, error))
                if (auto path = fromRoot(entry.path()); !path.empty())
                    candidates.push_back(path);
        if (!candidates.empty()) {
            std::sort(candidates.begin(), candidates.end());
            return candidates.back();
        }
        return L"Rscript.exe";
    }

    std::wstring QuoteProcessArgument(std::wstring const& value)
    {
        return L"\"" + value + L"\"";
    }

    bool SavePublicationPdf(
        std::vector<::rlispstat::core::PublicationTableSpec> const& tables,
        std::wstring const& destination, std::string const& number,
        std::string const& title, std::string& errorMessage)
    {
        namespace fs = std::filesystem;
        const auto directory = fs::temp_directory_path() /
            (L"linkeda-apa-" + std::to_wstring(GetCurrentProcessId()) + L"-" +
             std::to_wstring(GetTickCount64()) + L"-" +
             std::to_wstring(GetCurrentThreadId()));
        std::error_code error;
        if (!fs::create_directories(directory, error) || error) {
            errorMessage = "Could not create the temporary APA table directory.";
            return false;
        }
        const auto cleanup = [&]() { std::error_code ignored; fs::remove_all(directory, ignored); };
        ::rlispstat::core::LatexPublicationOptions options;
        options.apa7 = true;
        options.tableNumber = number;
        options.title = title;
        const auto scriptPath = directory / L"publication.R";
        {
            std::ofstream script(scriptPath, std::ios::binary);
            // Keep TeX font caches inside this temporary export directory.
            script << "Sys.setenv(TEXMFVAR = getwd(), TEXMFCACHE = getwd())\n";
            script << ::rlispstat::core::BuildLatexPublicationRCode(tables, true, options);
            if (!script.good()) {
                errorMessage = "Could not write the temporary APA table script.";
                cleanup();
                return false;
            }
        }
        const auto logPath = directory / L"publication.log";
        SECURITY_ATTRIBUTES security{sizeof(SECURITY_ATTRIBUTES), nullptr, TRUE};
        HANDLE log = CreateFileW(logPath.c_str(), GENERIC_WRITE, FILE_SHARE_READ,
            &security, CREATE_ALWAYS, FILE_ATTRIBUTE_TEMPORARY, nullptr);
        if (log == INVALID_HANDLE_VALUE) {
            errorMessage = "Could not create the APA table build log.";
            cleanup();
            return false;
        }
        STARTUPINFOW startup{};
        startup.cb = sizeof(startup);
        startup.dwFlags = STARTF_USESTDHANDLES;
        startup.hStdOutput = log;
        startup.hStdError = log;
        startup.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
        PROCESS_INFORMATION process{};
        const auto rscript = PublicationRscriptPath();
        std::wstring command = QuoteProcessArgument(rscript.wstring()) + L" --vanilla " +
            QuoteProcessArgument(scriptPath.wstring());
        std::vector<wchar_t> mutableCommand(command.begin(), command.end());
        mutableCommand.push_back(L'\0');
        const BOOL started = CreateProcessW(
            rscript.is_absolute() ? rscript.c_str() : nullptr,
            mutableCommand.data(), nullptr, nullptr, TRUE, CREATE_NO_WINDOW,
            nullptr, directory.c_str(), &startup, &process);
        CloseHandle(log);
        if (!started) {
            errorMessage = "Could not start Rscript to compile the APA table.";
            cleanup();
            return false;
        }
        const DWORD waited = WaitForSingleObject(process.hProcess, 300000);
        if (waited == WAIT_TIMEOUT) TerminateProcess(process.hProcess, 1);
        DWORD exitCode = 1;
        GetExitCodeProcess(process.hProcess, &exitCode);
        CloseHandle(process.hThread);
        CloseHandle(process.hProcess);
        if (waited == WAIT_OBJECT_0 && exitCode == 0 &&
            fs::exists(directory / L"linkeda-table.pdf", error))
        {
            fs::copy_file(directory / L"linkeda-table.pdf", destination,
                fs::copy_options::overwrite_existing, error);
            if (!error) { cleanup(); return true; }
        }
        std::ifstream input(logPath, std::ios::binary);
        std::ostringstream captured; captured << input.rdbuf();
        errorMessage = captured.str();
        if (errorMessage.size() > 600) errorMessage = errorMessage.substr(errorMessage.size() - 600);
        if (errorMessage.empty()) errorMessage = waited == WAIT_TIMEOUT
            ? "The APA table compilation timed out." : "The APA table PDF could not be compiled.";
        cleanup();
        return false;
    }

    void Report(Window const& owner, wchar_t const* message, bool error)
    {
        MessageBoxW(WindowHandle(owner), message, L"Export Table",
                    MB_OK | (error ? MB_ICONERROR : MB_ICONINFORMATION));
    }

    std::string FollowingTableNumber(std::string const& value)
    {
        if (!value.empty() && std::all_of(value.begin(), value.end(),
            [](unsigned char character) { return std::isdigit(character); }))
        {
            try { return std::to_string(std::stoull(value) + 1); }
            catch (...) {}
        }
        return value + "b";
    }

    std::string SectionTableTitle(std::string const& base,
                                  ::rlispstat::core::PublicationTableSpec const& table)
    {
        if (base.empty()) return table.title;
        if (table.title.empty()) return base;
        return base + ": " + table.title;
    }
}

namespace winrt::LinkEDA::implementation
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
            item.Text(action.command == "COPY_RICH"
                ? L"Copy APA 7 table..."
                : (action.command == "COPY_FORMATTED_TEXT"
                    ? L"Copy formatted table"
                    : to_hstring(action.title)));
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
        if (command == "COPY_RICH") ExportApaAsync(payload, true);
        else if (command == "COPY_FORMATTED_TEXT") CopyFormatted(payload);
        else if (command == "COPY_TAB_DELIMITED_TEXT") CopyTabDelimited(payload);
        else if (command == "COPY_SVG") CopyImageAsync(payload, true);
        else if (command == "COPY_PNG") CopyImageAsync(payload, false);
        else if (command == "SAVE_CSV") SaveText(payload, "csv");
        else if (command == "SAVE_MARKDOWN") SaveText(payload, "markdown");
        else if (command == "SAVE_APA_PDF") ExportApaAsync(payload, false);
        else if (command == "SAVE_SVG" || command == "SAVE_PDF" || command == "SAVE_PNG" ||
                 command == "SAVE_DISPLAYED_PDF")
            SaveVisualAsync(payload,
                command == "SAVE_SVG" ? "svg" :
                (command == "SAVE_PNG" ? "png" : "pdf"));
    }

    void TableExportService::CopyFormatted(TableExportPayload const& payload)
    {
        Windows::ApplicationModel::DataTransfer::DataPackage package;
        package.SetText(to_hstring(TableText(payload, true, true)));
        package.SetHtmlFormat(Windows::ApplicationModel::DataTransfer::HtmlFormatHelper::
            CreateHtmlFormat(to_hstring(TableHtml(payload, false))));
        Windows::ApplicationModel::DataTransfer::Clipboard::SetContent(package);
        Windows::ApplicationModel::DataTransfer::Clipboard::Flush();
    }

    fire_and_forget TableExportService::ExportApaAsync(
        TableExportPayload source, bool copyToClipboard)
    {
        auto lifetime = shared_from_this();
        auto root = owner_.Content().try_as<FrameworkElement>();
        if (!root || !root.XamlRoot())
        {
            Report(owner_, L"The APA 7 table details dialog could not be opened.", true);
            co_return;
        }

        auto fields = Controls::StackPanel();
        fields.Width(420.0); fields.Spacing(6.0);
        auto explanation = Controls::TextBlock();
        explanation.Text(L"Enter the table number and title.");
        explanation.TextWrapping(TextWrapping::Wrap);
        fields.Children().Append(explanation);
        auto numberLabel = Controls::TextBlock(); numberLabel.Text(L"Table number");
        fields.Children().Append(numberLabel);
        auto number = Controls::TextBox(); number.Text(L"1");
        fields.Children().Append(number);
        auto titleLabel = Controls::TextBlock(); titleLabel.Text(L"Table title");
        fields.Children().Append(titleLabel);
        auto title = Controls::TextBox();
        title.Text(to_hstring(::rlispstat::core::PublicationTitleWithoutTableLabel(source.title)));
        fields.Children().Append(title);
        Controls::ComboBox globalLayout{nullptr};
        if (!source.alternatePublicationTables.empty())
        {
            auto layoutLabel = Controls::TextBlock();
            layoutLabel.Text(L"Scale-level results");
            fields.Children().Append(layoutLabel);
            globalLayout = Controls::ComboBox();
            globalLayout.HorizontalAlignment(HorizontalAlignment::Stretch);
            globalLayout.Items().Append(box_value(L"Compact table above item results"));
            globalLayout.Items().Append(box_value(L"Note below item results"));
            globalLayout.SelectedIndex(0);
            fields.Children().Append(globalLayout);
        }
        Controls::ComboBox dimensionalityLayout{nullptr};
        if (!copyToClipboard && source.separatePublicationTables.size() == 2)
        {
            auto layoutLabel = Controls::TextBlock();
            layoutLabel.Text(L"Dimensionality tables");
            fields.Children().Append(layoutLabel);
            dimensionalityLayout = Controls::ComboBox();
            dimensionalityLayout.HorizontalAlignment(HorizontalAlignment::Stretch);
            dimensionalityLayout.Items().Append(box_value(L"One combined table in one PDF"));
            dimensionalityLayout.Items().Append(box_value(L"Two tables in two PDF files"));
            dimensionalityLayout.SelectedIndex(0);
            fields.Children().Append(dimensionalityLayout);
            auto separateHelp = Controls::TextBlock();
            separateHelp.Text(L"With two files, this is the first table number; the second follows it.");
            separateHelp.TextWrapping(TextWrapping::Wrap);
            fields.Children().Append(separateHelp);
        }

        auto dialog = Controls::ContentDialog();
        dialog.XamlRoot(root.XamlRoot());
        dialog.Title(box_value(L"APA 7 table details"));
        dialog.Content(fields);
        dialog.PrimaryButtonText(copyToClipboard ? L"Copy" : L"Export");
        dialog.CloseButtonText(L"Cancel");
        dialog.DefaultButton(Controls::ContentDialogButton::Primary);
        owner_.Activate();
        if (co_await dialog.ShowAsync() != Controls::ContentDialogResult::Primary) co_return;

        const std::string tableNumber = to_string(number.Text()).empty()
            ? "1" : to_string(number.Text());
        const std::string tableTitle = to_string(title.Text());
        if (!source.publicationTables.empty())
        {
            auto const& publicationTables = globalLayout && globalLayout.SelectedIndex() == 1
                ? source.alternatePublicationTables : source.publicationTables;
            if (copyToClipboard)
            {
                Windows::ApplicationModel::DataTransfer::DataPackage package;
                package.SetText(to_hstring(PublicationReportText(
                    publicationTables, tableNumber, tableTitle)));
                package.SetHtmlFormat(Windows::ApplicationModel::DataTransfer::HtmlFormatHelper::
                    CreateHtmlFormat(to_hstring(PublicationReportHtml(
                        publicationTables, tableNumber, tableTitle))));
                Windows::ApplicationModel::DataTransfer::Clipboard::SetContent(package);
                Windows::ApplicationModel::DataTransfer::Clipboard::Flush();
                co_return;
            }
            if (dimensionalityLayout && dimensionalityLayout.SelectedIndex() == 1)
            {
                const auto& separate = source.separatePublicationTables;
                const auto firstPath = ChoosePath(owner_,
                    source.baseName + "-" + separate[0].title + "_APA7", "pdf");
                if (!firstPath) co_return;
                const auto secondPath = ChoosePath(owner_,
                    source.baseName + "-" + separate[1].title + "_APA7", "pdf");
                if (!secondPath) co_return;
                auto normalizedFirst = std::filesystem::path(*firstPath).lexically_normal().wstring();
                auto normalizedSecond = std::filesystem::path(*secondPath).lexically_normal().wstring();
                if (_wcsicmp(normalizedFirst.c_str(), normalizedSecond.c_str()) == 0)
                {
                    Report(owner_, L"Choose two different PDF files for the dimensionality tables.", true);
                    co_return;
                }
                const auto dispatcher = renderer_.DispatcherQueue();
                std::string message;
                bool ok = false;
                co_await winrt::resume_background();
                namespace fs = std::filesystem;
                const auto staging = fs::temp_directory_path() /
                    (L"linkeda-apa-pair-" + std::to_wstring(GetCurrentProcessId()) + L"-" +
                     std::to_wstring(GetTickCount64()));
                std::error_code fileError;
                fs::create_directories(staging, fileError);
                const auto firstStaged = staging / L"first.pdf";
                const auto secondStaged = staging / L"second.pdf";
                auto firstTable = separate[0];
                auto secondTable = separate[1];
                const auto firstTitle = SectionTableTitle(tableTitle, firstTable);
                const auto secondTitle = SectionTableTitle(tableTitle, secondTable);
                firstTable.title.clear();
                secondTable.title.clear();
                if (!fileError && SavePublicationPdf({firstTable}, firstStaged.wstring(),
                        tableNumber, firstTitle, message) &&
                    SavePublicationPdf({secondTable}, secondStaged.wstring(),
                        FollowingTableNumber(tableNumber), secondTitle, message))
                {
                    fs::copy_file(firstStaged, *firstPath, fs::copy_options::overwrite_existing, fileError);
                    if (!fileError)
                        fs::copy_file(secondStaged, *secondPath,
                            fs::copy_options::overwrite_existing, fileError);
                    ok = !fileError;
                    if (!ok) message = "The two APA 7 PDFs could not both be written.";
                }
                else if (message.empty()) message = "The two APA 7 PDFs could not be compiled.";
                std::error_code ignored;
                fs::remove_all(staging, ignored);
                co_await wil::resume_foreground(dispatcher);
                if (!ok) Report(owner_, to_hstring(message).c_str(), true);
                owner_.Activate();
                co_return;
            }
            const auto path = ChoosePath(owner_, source.baseName + "_APA7", "pdf");
            if (!path) co_return;
            std::string message;
            const auto dispatcher = renderer_.DispatcherQueue();
            co_await winrt::resume_background();
            const bool ok = SavePublicationPdf(publicationTables, *path,
                tableNumber, tableTitle, message);
            co_await wil::resume_foreground(dispatcher);
            if (!ok) Report(owner_, to_hstring(message).c_str(), true);
            owner_.Activate();
            co_return;
        }
        const TableExportPayload payload = ApaPayload(std::move(source));
        if (copyToClipboard)
        {
            Windows::ApplicationModel::DataTransfer::DataPackage package;
            const std::string plain = "Table " + tableNumber + "\n" + tableTitle +
                "\n\n" + TableText(payload, false, true);
            package.SetText(to_hstring(plain));
            package.SetHtmlFormat(Windows::ApplicationModel::DataTransfer::HtmlFormatHelper::
                CreateHtmlFormat(to_hstring(TableHtml(
                    payload, true, tableNumber, tableTitle))));
            Windows::ApplicationModel::DataTransfer::Clipboard::SetContent(package);
            Windows::ApplicationModel::DataTransfer::Clipboard::Flush();
            co_return;
        }

        const auto path = ChoosePath(owner_, payload.baseName + "_APA7", "pdf");
        if (!path) co_return;
        bool ok = false;
        try
        {
            const auto document = StyledTableSvg(
                payload, true, tableNumber, tableTitle);
            ok = co_await RenderSvgToPdfAsync(document.svg, *path,
                document.dimensions.widthPoints, document.dimensions.heightPoints, true);
        }
        catch (...) { renderer_.Visibility(Visibility::Collapsed); }
        if (!ok) Report(owner_, L"The APA 7 table could not be exported.", true);
        owner_.Activate();
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
        TableExportPayload payload, std::string format)
    {
        auto lifetime = shared_from_this();
        const auto path = ChoosePath(owner_, payload.baseName, format);
        if (!path) co_return;
        bool ok = false;
        try
        {
            const auto document = StyledTableSvg(payload, false);
            if (format == "svg") ok = WriteText(*path, document.svg);
            else if (format == "pdf") ok = co_await RenderSvgToPdfAsync(document.svg, *path,
                document.dimensions.widthPoints, document.dimensions.heightPoints, false);
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
