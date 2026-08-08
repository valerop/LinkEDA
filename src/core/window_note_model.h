#pragma once

#include <chrono>
#include <cstddef>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

namespace rlispstat::core {

using WindowNoteTimePoint = std::chrono::system_clock::time_point;

struct WindowNote {
    std::string view_id;
    std::string plain_text;
    bool visible = false;
    bool has_content = false;
    bool include_in_export = false;
    WindowNoteTimePoint created_at{};
    WindowNoteTimePoint modified_at{};
};

enum class StickyNoteColor {
    Yellow,
    Pink,
    Blue,
    Green,
    Orange
};

struct WindowStickyNote {
    std::string note_id;
    std::string plain_text;
    StickyNoteColor color = StickyNoteColor::Yellow;
    double x = 24.0;
    double y = 24.0;
    double width = 220.0;
    double height = 160.0;
    double anchor_x = 12.0;
    double anchor_y = 220.0;
    bool has_content = false;
    WindowNoteTimePoint created_at{};
    WindowNoteTimePoint modified_at{};
};

struct WindowNoteExportContent {
    std::string heading = "Notes";
    std::string plain_text;
};

// Portable description consumed by platform-specific visual exporters.  The
// primary renderer remains owned by the platform; Notes is an optional layer.
struct WindowExportComposition {
    std::string view_id;
    std::optional<WindowNoteExportContent> note;
};

bool WindowNoteTextHasContent(const std::string &text);
std::string GenerateWindowNoteViewId();
std::string GenerateWindowStickyNoteId();
WindowNote MakeWindowNote(const std::string &viewId);
WindowStickyNote MakeWindowStickyNote(const std::string &noteId,
                                      StickyNoteColor color = StickyNoteColor::Yellow);
void SetWindowStickyNoteText(WindowStickyNote &note, const std::string &text,
                             WindowNoteTimePoint now = std::chrono::system_clock::now());
void SetWindowStickyNoteColor(WindowStickyNote &note, StickyNoteColor color);
void MoveWindowStickyNote(WindowStickyNote &note, double x, double y);
void ResizeWindowStickyNote(WindowStickyNote &note, double width, double height);
void MoveWindowStickyNoteAnchor(WindowStickyNote &note, double x, double y);
void SetWindowNoteText(WindowNote &note, const std::string &text,
                       WindowNoteTimePoint now = std::chrono::system_clock::now());
void SetWindowNoteVisible(WindowNote &note, bool visible);
void SetWindowNoteIncludedInExport(WindowNote &note, bool included);
void ClearWindowNote(WindowNote &note,
                     WindowNoteTimePoint now = std::chrono::system_clock::now());
WindowExportComposition BuildWindowExportComposition(const WindowNote &note);
std::string ComposeSvgDocumentWithWindowNote(const std::string &svg,
                                             const WindowNoteExportContent &note);
std::string ComposeSvgDocumentWithWindowStickyNotes(
    const std::string &svg, const std::vector<WindowStickyNote> &stickers);

class WindowNoteStore {
public:
    WindowNote &create(const std::string &viewId);
    WindowNote *find(const std::string &viewId);
    const WindowNote *find(const std::string &viewId) const;
    bool erase(const std::string &viewId);
    std::size_t size() const;

private:
    std::unordered_map<std::string, WindowNote> notes_;
};

} // namespace rlispstat::core
