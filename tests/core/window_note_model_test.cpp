#include "../../src/core/window_note_model.h"

#include <cassert>
#include <chrono>
#include <string>

using namespace rlispstat::core;

int main()
{
    const auto created = WindowNoteTimePoint(std::chrono::seconds(10));
    const auto modified = WindowNoteTimePoint(std::chrono::seconds(20));
    WindowNote note = MakeWindowNote("view-a");
    assert(note.view_id == "view-a");
    assert(!note.has_content && !note.visible && !note.include_in_export);
    assert(note.created_at == WindowNoteTimePoint{});

    SetWindowNoteText(note, "Relationship appears approximately linear.", created);
    assert(note.has_content);
    assert(note.created_at == created && note.modified_at == created);
    SetWindowNoteText(note, "Relationship changed.\nCheck observation 18.", modified);
    assert(note.created_at == created && note.modified_at == modified);
    assert(note.plain_text.find('\n') != std::string::npos);

    SetWindowNoteVisible(note, true);
    SetWindowNoteVisible(note, false);
    assert(note.has_content && !note.visible);
    SetWindowNoteVisible(note, true);
    assert(note.plain_text.find("observation 18") != std::string::npos);

    SetWindowNoteIncludedInExport(note, true);
    auto composition = BuildWindowExportComposition(note);
    assert(composition.note.has_value());
    assert(composition.note->heading == "Notes");
    SetWindowNoteIncludedInExport(note, false);
    assert(!BuildWindowExportComposition(note).note.has_value());

    SetWindowNoteIncludedInExport(note, true);
    const std::string svg = "<svg width='400pt' height='300pt' viewBox='0 0 400 300'><path d='M0 0'/></svg>";
    const std::string composedSvg = ComposeSvgDocumentWithWindowNote(
        svg, WindowNoteExportContent{"Notes", u8"Line <one> & dos\nSegunda línea"});
    assert(composedSvg.find("linkeda-window-notes") != std::string::npos);
    assert(composedSvg.find("&lt;one&gt; &amp; dos") != std::string::npos);
    assert(composedSvg.find("viewBox='0 0 400 300'") == std::string::npos);

    SetWindowNoteText(note, " \t\n ", modified);
    assert(!note.has_content && !note.include_in_export);
    SetWindowNoteText(note, u8"Unicode: relación — 日本語\nSecond paragraph", modified);
    assert(note.has_content);
    std::string longText(200000, 'x');
    SetWindowNoteText(note, longText, modified);
    assert(note.plain_text.size() == longText.size());

    WindowNoteStore store;
    WindowNote &first = store.create("same-graph-a");
    WindowNote &second = store.create("same-graph-b");
    SetWindowNoteText(first, "All observations", created);
    SetWindowNoteText(second, "Scope cyl = 4", created);
    assert(first.plain_text != second.plain_text);
    assert(store.size() == 2);

    // Selection, scope, theme, and trellis content are deliberately external
    // to WindowNote; changing them cannot mutate a note.
    int selectionVersion = 3;
    std::string scope = "all";
    std::string theme = "manet";
    std::string trellisContent = "scatterplot";
    const std::string retained = second.plain_text;
    ++selectionVersion; scope = "selected"; theme = "vista"; trellisContent = "data_table";
    assert(selectionVersion == 4 && scope == "selected" && theme == "vista");
    assert(trellisContent == "data_table" && second.plain_text == retained);

    assert(store.erase("same-graph-a"));
    assert(store.find("same-graph-a") == nullptr && store.size() == 1);
    ClearWindowNote(second, modified);
    assert(!second.has_content && second.plain_text.empty());

    const std::string generatedA = GenerateWindowNoteViewId();
    const std::string generatedB = GenerateWindowNoteViewId();
    assert(generatedA.rfind("view-", 0) == 0);
    assert(generatedA != generatedB);

    WindowStickyNote sticky = MakeWindowStickyNote(
        GenerateWindowStickyNoteId(), StickyNoteColor::Yellow);
    assert(sticky.note_id.rfind("sticky-", 0) == 0);
    assert(sticky.color == StickyNoteColor::Yellow && !sticky.has_content);
    SetWindowStickyNoteText(sticky, u8"Revisar este punto — 日本語", created);
    assert(sticky.has_content && sticky.created_at == created);
    SetWindowStickyNoteColor(sticky, StickyNoteColor::Pink);
    MoveWindowStickyNote(sticky, 120.0, 80.0);
    ResizeWindowStickyNote(sticky, 280.0, 190.0);
    MoveWindowStickyNoteAnchor(sticky, 32.0, 250.0);
    assert(sticky.color == StickyNoteColor::Pink);
    assert(sticky.x == 120.0 && sticky.y == 80.0);
    assert(sticky.width == 280.0 && sticky.height == 190.0);
    assert(sticky.anchor_x == 32.0 && sticky.anchor_y == 250.0);
    const std::string stickySvg = ComposeSvgDocumentWithWindowStickyNotes(svg, {sticky});
    assert(stickySvg.find("linkeda-snapshot-stickers") != std::string::npos);
    assert(stickySvg.find("#ffb8d6") != std::string::npos);
    assert(stickySvg.find("Revisar este punto") != std::string::npos);
    assert(stickySvg.find("x2='32'") != std::string::npos);
    return 0;
}
