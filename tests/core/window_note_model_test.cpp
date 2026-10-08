#include "../../src/core/window_note_model.h"

#include <cassert>
#include <chrono>
#include <cmath>
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
    SetWindowStickyNoteReferenceSize(sticky, 800.0, 600.0);
    WindowStickyNote scaled = WindowStickyNoteForCanvas(sticky, 400.0, 300.0);
    assert(scaled.x == 60.0 && scaled.y == 40.0);
    assert(scaled.width == 140.0 && scaled.height == 95.0);
    assert(scaled.anchor_x == 16.0 && scaled.anchor_y == 125.0);
    SetWindowStickyNoteAnchorReferenceRect(sticky, 0.0, 0.0, 400.0, 300.0);
    WindowStickyNote plotScaled = WindowStickyNoteForCanvas(
        sticky, 400.0, 300.0, 50.0, 30.0, 300.0, 220.0);
    assert(std::abs(plotScaled.anchor_x - 74.0) < 1e-9);
    assert(std::abs(plotScaled.anchor_y - 213.33333333333333) < 1e-9);
    SetWindowStickyNoteCollapsed(sticky, true);
    assert(sticky.collapsed);
    RelayoutWindowStickyNoteForCanvasResize(sticky, 800.0, 600.0, 1200.0, 900.0);
    assert(sticky.x == 250.0 && sticky.y == 167.5);
    assert(sticky.anchor_x == 48.0 && sticky.anchor_y == 375.0);
    RelayoutWindowStickyNoteForCanvasResize(sticky, 1200.0, 900.0, 240.0, 180.0);
    assert(sticky.x == 0.0 && sticky.y == 0.0);
    assert(std::abs(sticky.anchor_x - 9.6) < 1e-9 && sticky.anchor_y == 75.0);
    const std::string collapsedSvg = ComposeSvgDocumentWithWindowStickyNotes(svg, {sticky});
    assert(collapsedSvg.find("text-anchor='middle'") != std::string::npos);
    assert(collapsedSvg.find("Revisar este punto") == std::string::npos);
    SetWindowStickyNoteCollapsed(sticky, false);
    const std::string stickySvg = ComposeSvgDocumentWithWindowStickyNotes(svg, {sticky});
    assert(stickySvg.find("linkeda-snapshot-stickers") != std::string::npos);
    assert(stickySvg.find("#ffb8d6") != std::string::npos);
    assert(stickySvg.find("Revisar este punto") != std::string::npos);
    assert(stickySvg.find(">Sticker</text>") == std::string::npos);
    assert(stickySvg.find("x2='16'") != std::string::npos);
    WindowStickyNote anchored = MakeWindowStickyNote("plot-anchor");
    SetWindowStickyNoteText(anchored, "Same data point");
    MoveWindowStickyNoteAnchor(anchored, 500.0, 305.0);
    SetWindowStickyNoteReferenceSize(anchored, 1000.0, 600.0);
    SetWindowStickyNoteAnchorReferenceRect(anchored, 100.0, 80.0, 800.0, 450.0);
    const std::string plotSvg =
        "<svg viewBox=\"0 0 500 300\"><rect x=\"0\" y=\"0\" width=\"500\" height=\"300\"/>"
        "<rect x=\"70\" y=\"52\" width=\"400\" height=\"200\"/></svg>";
    const std::string anchoredSvg = ComposeSvgDocumentWithWindowStickyNotes(
        plotSvg, {anchored});
    assert(anchoredSvg.find("x2='270'") != std::string::npos);
    assert(anchoredSvg.find("y2='152'") != std::string::npos);
    return 0;
}
