#include "window_note_model.h"

#include <algorithm>
#include <array>
#include <atomic>
#include <cctype>
#include <iomanip>
#include <random>
#include <regex>
#include <sstream>
#include <vector>

namespace rlispstat::core {

bool WindowNoteTextHasContent(const std::string &text)
{
    for (unsigned char ch : text) {
        if (!std::isspace(ch)) return true;
    }
    return false;
}

std::string GenerateWindowNoteViewId()
{
    // UUID-shaped random identity: it is independent of titles, datasets,
    // window positions, plot specifications, pointers, and opening order.
    std::array<unsigned char, 16> bytes{};
    std::random_device random;
    for (unsigned char &byte : bytes) byte = static_cast<unsigned char>(random());
    bytes[6] = static_cast<unsigned char>((bytes[6] & 0x0fU) | 0x40U);
    bytes[8] = static_cast<unsigned char>((bytes[8] & 0x3fU) | 0x80U);

    std::ostringstream out;
    out << "view-" << std::hex << std::setfill('0');
    for (std::size_t index = 0; index < bytes.size(); ++index) {
        if (index == 4 || index == 6 || index == 8 || index == 10) out << '-';
        out << std::setw(2) << static_cast<unsigned int>(bytes[index]);
    }
    return out.str();
}

std::string GenerateWindowStickyNoteId()
{
    std::string id = GenerateWindowNoteViewId();
    id.replace(0, 5, "sticky-");
    return id;
}

WindowNote MakeWindowNote(const std::string &viewId)
{
    WindowNote note;
    note.view_id = viewId;
    return note;
}

WindowStickyNote MakeWindowStickyNote(const std::string &noteId, StickyNoteColor color)
{
    WindowStickyNote note;
    note.note_id = noteId;
    note.color = color;
    return note;
}

void SetWindowStickyNoteText(WindowStickyNote &note, const std::string &text,
                             WindowNoteTimePoint now)
{
    const bool hadContent = note.has_content;
    note.plain_text = text;
    note.has_content = WindowNoteTextHasContent(text);
    if (note.has_content && !hadContent) note.created_at = now;
    if (note.has_content || hadContent) note.modified_at = now;
}

void SetWindowStickyNoteColor(WindowStickyNote &note, StickyNoteColor color)
{
    note.color = color;
}

void MoveWindowStickyNote(WindowStickyNote &note, double x, double y)
{
    note.x = x;
    note.y = y;
}

void ResizeWindowStickyNote(WindowStickyNote &note, double width, double height)
{
    note.width = std::max(160.0, width);
    note.height = std::max(110.0, height);
}

void MoveWindowStickyNoteAnchor(WindowStickyNote &note, double x, double y)
{
    note.anchor_x = x;
    note.anchor_y = y;
}

void SetWindowNoteText(WindowNote &note, const std::string &text, WindowNoteTimePoint now)
{
    const bool hadContent = note.has_content;
    note.plain_text = text;
    note.has_content = WindowNoteTextHasContent(text);
    if (note.has_content && !hadContent) note.created_at = now;
    if (note.has_content || hadContent) note.modified_at = now;
    if (!note.has_content) note.include_in_export = false;
}

void SetWindowNoteVisible(WindowNote &note, bool visible)
{
    note.visible = visible;
}

void SetWindowNoteIncludedInExport(WindowNote &note, bool included)
{
    note.include_in_export = included && note.has_content;
}

void ClearWindowNote(WindowNote &note, WindowNoteTimePoint now)
{
    const bool hadContent = note.has_content;
    note.plain_text.clear();
    note.has_content = false;
    note.include_in_export = false;
    if (hadContent) note.modified_at = now;
}

WindowExportComposition BuildWindowExportComposition(const WindowNote &note)
{
    WindowExportComposition composition;
    composition.view_id = note.view_id;
    if (note.has_content && note.include_in_export) {
        composition.note = WindowNoteExportContent{"Notes", note.plain_text};
    }
    return composition;
}

namespace {

std::string EscapeSvgText(const std::string &text)
{
    std::string escaped;
    escaped.reserve(text.size());
    for (char ch : text) {
        switch (ch) {
        case '&': escaped += "&amp;"; break;
        case '<': escaped += "&lt;"; break;
        case '>': escaped += "&gt;"; break;
        case '\"': escaped += "&quot;"; break;
        case '\'': escaped += "&apos;"; break;
        default: escaped += ch; break;
        }
    }
    return escaped;
}

std::vector<std::string> WrapSvgNote(const std::string &text, std::size_t columns)
{
    std::vector<std::string> lines;
    std::istringstream paragraphs(text);
    std::string paragraph;
    while (std::getline(paragraphs, paragraph)) {
        if (paragraph.empty()) {
            lines.emplace_back();
            continue;
        }
        std::istringstream words(paragraph);
        std::string word;
        std::string line;
        while (words >> word) {
            if (!line.empty() && line.size() + 1 + word.size() > columns) {
                lines.push_back(line);
                line.clear();
            }
            if (!line.empty()) line += ' ';
            line += word;
        }
        lines.push_back(line);
    }
    if (lines.empty()) lines.emplace_back();
    return lines;
}

} // namespace

std::string ComposeSvgDocumentWithWindowNote(const std::string &svg,
                                             const WindowNoteExportContent &note)
{
    if (!WindowNoteTextHasContent(note.plain_text)) return svg;
    std::smatch viewBoxMatch;
    const std::regex viewBoxPattern(R"(viewBox\s*=\s*['\"]\s*([-+0-9.eE]+)\s+([-+0-9.eE]+)\s+([-+0-9.eE]+)\s+([-+0-9.eE]+)\s*['\"])");
    if (!std::regex_search(svg, viewBoxMatch, viewBoxPattern)) return svg;
    const double x = std::stod(viewBoxMatch[1].str());
    const double y = std::stod(viewBoxMatch[2].str());
    const double width = std::stod(viewBoxMatch[3].str());
    const double height = std::stod(viewBoxMatch[4].str());
    if (!(width > 0.0) || !(height > 0.0)) return svg;

    const double usableWidth = std::max(140.0, width - 48.0);
    const std::size_t columns = std::max<std::size_t>(20, static_cast<std::size_t>(usableWidth / 7.0));
    const std::vector<std::string> lines = WrapSvgNote(note.plain_text, columns);
    const double extraHeight = 58.0 + 17.0 * static_cast<double>(lines.size());
    const double totalHeight = height + extraHeight;

    std::ostringstream replacement;
    replacement << "viewBox='" << x << " " << y << " " << width << " " << totalHeight << "'";
    std::string composed = std::regex_replace(svg, viewBoxPattern, replacement.str(),
                                               std::regex_constants::format_first_only);
    const std::regex heightPattern(R"(height\s*=\s*['\"]([-+0-9.eE]+)(pt|px)?['\"])");
    if (std::regex_search(composed, heightPattern)) {
        std::ostringstream heightReplacement;
        heightReplacement << "height='" << totalHeight << "pt'";
        composed = std::regex_replace(composed, heightPattern, heightReplacement.str(),
                                      std::regex_constants::format_first_only);
    }

    const std::size_t closing = composed.rfind("</svg>");
    if (closing == std::string::npos) return svg;
    std::ostringstream block;
    block << "<g id='linkeda-window-notes' font-family='sans-serif' fill='#111111'>\n"
          << "<rect x='0' y='" << height << "' width='" << width
          << "' height='" << extraHeight << "' fill='#ffffff'/>\n"
          << "<text x='24' y='" << (height + 30.0)
          << "' font-size='13' font-weight='600'>" << EscapeSvgText(note.heading) << "</text>\n"
          << "<text x='24' y='" << (height + 54.0) << "' font-size='12'>\n";
    for (std::size_t index = 0; index < lines.size(); ++index) {
        block << "<tspan x='24' dy='" << (index == 0 ? 0 : 17) << "'>"
              << EscapeSvgText(lines[index]) << "</tspan>\n";
    }
    block << "</text>\n</g>\n";
    composed.insert(closing, block.str());
    return composed;
}

std::string ComposeSvgDocumentWithWindowStickyNotes(
    const std::string &svg, const std::vector<WindowStickyNote> &stickers)
{
    if (stickers.empty()) return svg;
    const std::size_t closing = svg.rfind("</svg>");
    if (closing == std::string::npos) return svg;
    const auto fillForColor = [](StickyNoteColor color) {
        switch (color) {
        case StickyNoteColor::Pink: return "#ffb8d6";
        case StickyNoteColor::Blue: return "#b8defa";
        case StickyNoteColor::Green: return "#c7f0b3";
        case StickyNoteColor::Orange: return "#ffcf99";
        case StickyNoteColor::Yellow: return "#fff08f";
        }
        return "#fff08f";
    };
    std::ostringstream group;
    group << "<g id='linkeda-snapshot-stickers' font-family='sans-serif'>\n";
    for (const WindowStickyNote &sticker : stickers) {
        if (!sticker.has_content) continue;
        const double centerX = sticker.x + sticker.width / 2.0;
        const double centerY = sticker.y + sticker.height / 2.0;
        group << "<line x1='" << centerX << "' y1='" << centerY
              << "' x2='" << sticker.anchor_x << "' y2='" << sticker.anchor_y
              << "' stroke='#383838' stroke-opacity='.72' stroke-width='1.5'/>\n"
              << "<circle cx='" << sticker.anchor_x << "' cy='" << sticker.anchor_y
              << "' r='5' fill='#333333' fill-opacity='.9'/>\n"
              << "<rect x='" << sticker.x << "' y='" << sticker.y
              << "' width='" << sticker.width << "' height='" << sticker.height
              << "' rx='5' fill='" << fillForColor(sticker.color)
              << "' stroke='#555555' stroke-opacity='.3'/>\n"
              << "<rect x='" << sticker.x << "' y='" << sticker.y
              << "' width='" << sticker.width << "' height='28' rx='5' fill='#000000' fill-opacity='.07'/>\n"
              << "<text x='" << (sticker.x + 8.0) << "' y='" << (sticker.y + 18.0)
              << "' font-size='11' font-weight='600' fill='#242424'>Sticker</text>\n";
        const std::size_t columns = std::max<std::size_t>(12,
            static_cast<std::size_t>(std::max(1.0, sticker.width - 24.0) / 7.0));
        std::vector<std::string> lines = WrapSvgNote(sticker.plain_text, columns);
        const std::size_t maxLines = std::max<std::size_t>(1,
            static_cast<std::size_t>(std::max(1.0, sticker.height - 48.0) / 17.0));
        if (lines.size() > maxLines) lines.resize(maxLines);
        group << "<text x='" << (sticker.x + 12.0) << "' y='" << (sticker.y + 51.0)
              << "' font-size='13' fill='#1a1a1a'>\n";
        for (std::size_t index = 0; index < lines.size(); ++index) {
            group << "<tspan x='" << (sticker.x + 12.0) << "' dy='"
                  << (index == 0 ? 0 : 17) << "'>" << EscapeSvgText(lines[index])
                  << "</tspan>\n";
        }
        group << "</text>\n";
    }
    group << "</g>\n";
    std::string composed = svg;
    composed.insert(closing, group.str());
    return composed;
}

WindowNote &WindowNoteStore::create(const std::string &viewId)
{
    auto [found, inserted] = notes_.emplace(viewId, MakeWindowNote(viewId));
    return found->second;
}

WindowNote *WindowNoteStore::find(const std::string &viewId)
{
    auto found = notes_.find(viewId);
    return found == notes_.end() ? nullptr : &found->second;
}

const WindowNote *WindowNoteStore::find(const std::string &viewId) const
{
    auto found = notes_.find(viewId);
    return found == notes_.end() ? nullptr : &found->second;
}

bool WindowNoteStore::erase(const std::string &viewId)
{
    return notes_.erase(viewId) > 0;
}

std::size_t WindowNoteStore::size() const
{
    return notes_.size();
}

} // namespace rlispstat::core
