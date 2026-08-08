#include "../../src/core/snapshot_album_model.h"

#include <cassert>
#include <chrono>
#include <string>

using namespace rlispstat::core;

static SnapshotItem PlotSnapshot(const std::string &sourceId, const std::string &title,
                                 double x, const std::string &theme)
{
    SnapshotItem item;
    item.content_kind = SnapshotContentKind::Plot;
    item.metadata.source_view_id = sourceId;
    item.metadata.dataset_id = "mtcars";
    item.metadata.title = title;
    item.metadata.default_title = title;
    item.metadata.source_description = "Scatterplot of wt by mpg";
    item.metadata.theme_name = theme;
    item.metadata.source_scope = ExplicitAnalysisScope(
        "mtcars", {1, 3, 8}, AnalysisScopeSourceKind::TrellisPanel,
        "cyl = 4", 32, sourceId);
    FrozenPlotContent plot;
    plot.model.kind = "scatter";
    plot.model.title = title;
    plot.model.points.push_back({x, 20.0, 1});
    plot.model.dataScope = item.metadata.source_scope;
    plot.model.dataScopeCaptured = true;
    item.renderable_content = plot;
    return item;
}

int main()
{
    SnapshotAlbum album;
    assert(album.empty() && album.size() == 0);

    WindowNote sourceNote = MakeWindowNote("source-view");
    SetWindowNoteText(sourceNote, "Interesting relation", WindowNoteTimePoint(std::chrono::seconds(10)));
    SetWindowNoteVisible(sourceNote, true);
    SnapshotItem first = PlotSnapshot("source-view", "Scatterplot", 1.0, "MANET");
    first.note = sourceNote;
    WindowStickyNote sourceSticker = MakeWindowStickyNote("source-sticker", StickyNoteColor::Pink);
    SetWindowStickyNoteText(sourceSticker, "Outlying point");
    MoveWindowStickyNote(sourceSticker, 140.0, 85.0);
    ResizeWindowStickyNote(sourceSticker, 260.0, 180.0);
    MoveWindowStickyNoteAnchor(sourceSticker, 72.0, 210.0);
    first.stickers.push_back(sourceSticker);
    SnapshotItem &stored = album.add(first);
    const std::string firstId = stored.metadata.snapshot_id;
    assert(firstId.rfind("snapshot-", 0) == 0);
    assert(stored.metadata.source_view_id == "source-view");
    assert(stored.metadata.source_scope.originalRowIds.size() == 3);
    assert(stored.metadata.theme_name == "MANET");
    assert(stored.note && stored.note->plain_text == "Interesting relation");
    assert(stored.note->view_id == firstId);
    assert(stored.note->visible);
    assert(stored.stickers.size() == 1);
    assert(stored.stickers[0].plain_text == "Outlying point");
    assert(stored.stickers[0].color == StickyNoteColor::Pink);
    assert(stored.stickers[0].x == 140.0 && stored.stickers[0].width == 260.0);
    assert(stored.stickers[0].anchor_y == 210.0);

    // Mutating or destroying the source values cannot change the stored copy.
    sourceNote.plain_text = "Changed later";
    sourceSticker.plain_text = "Changed sticker";
    first.stickers[0].plain_text = sourceSticker.plain_text;
    auto &sourcePlot = std::get<FrozenPlotContent>(first.renderable_content).model;
    sourcePlot.points[0].x = 999.0;
    assert(album.find(firstId)->note->plain_text == "Interesting relation");
    assert(album.find(firstId)->note->visible);
    assert(album.find(firstId)->stickers[0].plain_text == "Outlying point");
    assert(std::get<FrozenPlotContent>(album.find(firstId)->renderable_content).model.points[0].x == 1.0);

    assert(album.rename(firstId, "Renamed"));
    assert(album.find(firstId)->metadata.snapshot_id == firstId);
    assert(album.find(firstId)->metadata.title == "Renamed");
    assert(album.rename(firstId, "  \n"));
    assert(album.find(firstId)->metadata.title == "Scatterplot");

    SnapshotItem &second = album.add(PlotSnapshot("source-view", "Second state", 2.0, "ViSta"));
    const std::string secondId = second.metadata.snapshot_id;
    assert(firstId != secondId && album.size() == 2);
    assert(std::get<FrozenPlotContent>(album.find(secondId)->renderable_content).model.points[0].x == 2.0);
    assert(album.moveUp(secondId));
    assert(album.items()[0].metadata.snapshot_id == secondId);
    assert(album.moveDown(secondId));
    assert(album.items()[1].metadata.snapshot_id == secondId);

    assert(album.setIncludedInExport(firstId, false));
    assert(album.exportItems().size() == 1 && album.exportItems()[0]->metadata.snapshot_id == secondId);
    assert(album.setNoteText(secondId, u8"Álbum — 日本語"));
    assert(album.setNoteVisible(secondId, true));
    assert(album.find(secondId)->note->visible);
    assert(album.setNoteText(secondId, u8"Álbum ampliado"));
    assert(album.find(secondId)->note->visible);
    assert(album.setNotesIncluded(secondId, false));
    assert(!album.find(secondId)->note->include_in_export);
    assert(SnapshotAlbumManifestCSV(album).find(u8"Álbum — 日本語") == std::string::npos);
    assert(SnapshotAlbumManifestCSV(album).find("Second state") != std::string::npos);
    assert(EstimateSnapshotMemoryBytes(*album.find(secondId)) > sizeof(SnapshotItem));

    assert(album.remove(firstId));
    assert(album.remove(secondId));
    assert(album.empty());

    SnapshotItem table;
    table.content_kind = SnapshotContentKind::DataTable;
    table.metadata.title = "Visible data table";
    FrozenTableContent frozenTable;
    frozenTable.tab_delimited_text = "row\twt\tmpg\n1\t2.620\t21.0";
    frozenTable.first_source_row = 0;
    frozenTable.source_row_count = 1;
    frozenTable.total_source_row_count = 32;
    frozenTable.contains_all_source_rows = false;
    table.renderable_content = frozenTable;
    SnapshotItem &storedTable = album.add(table);
    assert(std::get<FrozenTableContent>(storedTable.renderable_content).contains_all_source_rows == false);
    assert(std::get<FrozenTableContent>(storedTable.renderable_content).total_source_row_count == 32);
    album.clear();
    assert(album.empty());
    return 0;
}
