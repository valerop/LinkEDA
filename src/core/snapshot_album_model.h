#pragma once

#include "analysis_scope.h"
#include "plot_geometry.h"
#include "window_note_model.h"

#include <chrono>
#include <cstddef>
#include <optional>
#include <string>
#include <variant>
#include <vector>

namespace rlispstat::core {

enum class SnapshotContentKind {
    Plot,
    StatisticalTable,
    DataTable,
    Trellis,
    ModelResult,
    TextOutput,
    OtherRenderable
};

struct FrozenViewSpecification {
    std::string view_type;
    std::vector<std::pair<std::string, std::string>> variables;
    std::vector<std::pair<std::string, std::string>> options;
};

struct FrozenPlotContent {
    PlotModel model;
    double preferred_width = 720.0;
    double preferred_height = 520.0;
};

struct FrozenTableContent {
    std::vector<std::string> column_headers;
    std::vector<std::vector<std::string>> rows;
    std::string tab_delimited_text;
    std::vector<double> column_widths;
    std::size_t first_source_row = 0;
    std::size_t source_row_count = 0;
    std::size_t total_source_row_count = 0;
    bool contains_all_source_rows = true;
};

struct FrozenTextContent {
    std::string plain_text;
};

// A self-contained vector rendering frozen from a native result surface.
// Unlike FrozenImageContent, text and rules remain sharp at every zoom level.
struct FrozenSvgContent {
    std::string svg;
    double preferred_width = 720.0;
    double preferred_height = 520.0;
};

// A rendered native view which has no portable PlotModel representation yet.
// The source image keeps such snapshots faithful while the window-specific
// renderer continues to evolve independently.
struct FrozenImageContent {
    std::vector<unsigned char> png_data;
    double preferred_width = 720.0;
    double preferred_height = 520.0;
};

using FrozenRenderableContent = std::variant<FrozenPlotContent, FrozenTableContent,
                                             FrozenSvgContent, FrozenImageContent,
                                             FrozenTextContent>;

struct SnapshotMetadata {
    std::string snapshot_id;
    std::string source_view_id;
    std::string dataset_id;
    std::string title;
    std::string default_title;
    std::string source_description;
    AnalysisScope source_scope;
    std::string theme_name = "Default";
    std::chrono::system_clock::time_point created_at{};
    bool include_in_export = true;
    bool include_notes = true;
};

struct SnapshotItem {
    SnapshotMetadata metadata;
    SnapshotContentKind content_kind = SnapshotContentKind::OtherRenderable;
    FrozenViewSpecification view_specification;
    FrozenRenderableContent renderable_content = FrozenTableContent{};
    std::optional<WindowNote> note;
    std::vector<WindowStickyNote> stickers;
};

std::string GenerateSnapshotId();
std::string SnapshotContentKindName(SnapshotContentKind kind);
std::string SnapshotScopeSummary(const SnapshotItem &item);
std::size_t EstimateSnapshotMemoryBytes(const SnapshotItem &item);
FrozenTableContent FrozenTableFromTabDelimited(std::string const& text);
std::string FrozenTableToTabDelimited(FrozenTableContent const& table);

class SnapshotAlbum {
public:
    const std::vector<SnapshotItem> &items() const;
    std::size_t size() const;
    bool empty() const;

    SnapshotItem &add(SnapshotItem item);
    SnapshotItem *find(const std::string &snapshotId);
    const SnapshotItem *find(const std::string &snapshotId) const;
    bool remove(const std::string &snapshotId);
    bool rename(const std::string &snapshotId, const std::string &title);
    bool move(const std::string &snapshotId, std::size_t destinationIndex);
    bool moveUp(const std::string &snapshotId);
    bool moveDown(const std::string &snapshotId);
    bool setIncludedInExport(const std::string &snapshotId, bool included);
    bool setNotesIncluded(const std::string &snapshotId, bool included);
    bool setNoteVisible(const std::string &snapshotId, bool visible);
    bool setNoteText(const std::string &snapshotId, const std::string &text,
                     WindowNoteTimePoint now = std::chrono::system_clock::now());
    std::vector<const SnapshotItem *> exportItems() const;
    void clear();

private:
    std::vector<SnapshotItem> items_;
};

std::string SnapshotAlbumManifestCSV(const SnapshotAlbum &album);

} // namespace rlispstat::core
