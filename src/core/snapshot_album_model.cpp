#include "snapshot_album_model.h"

#include <algorithm>
#include <array>
#include <cctype>
#include <iomanip>
#include <random>
#include <sstream>

namespace rlispstat::core {
namespace {

bool HasNonWhitespace(const std::string &text)
{
    return std::any_of(text.begin(), text.end(), [](unsigned char ch) {
        return !std::isspace(ch);
    });
}

std::string CsvCell(const std::string &text)
{
    std::string escaped;
    escaped.reserve(text.size() + 2);
    escaped.push_back('"');
    for (char ch : text) {
        if (ch == '"') escaped.push_back('"');
        escaped.push_back(ch);
    }
    escaped.push_back('"');
    return escaped;
}

std::size_t StringBytes(const std::string &value)
{
    return value.capacity();
}

std::size_t PlotMemoryBytes(const PlotModel &model)
{
    std::size_t bytes = sizeof(model);
    bytes += StringBytes(model.kind) + StringBytes(model.id) + StringBytes(model.group) +
        StringBytes(model.xLabel) + StringBytes(model.yLabel) + StringBytes(model.title);
    bytes += model.points.capacity() * sizeof(DataPoint);
    bytes += model.boxplotPoints.capacity() * sizeof(BoxplotPoint);
    bytes += model.histogramPoints.capacity() * sizeof(HistogramPoint);
    bytes += model.histogramBins.capacity() * sizeof(HistogramBin);
    bytes += model.barplotBins.capacity() * sizeof(BarplotBin);
    for (const auto &variable : model.variables)
        bytes += sizeof(variable) + StringBytes(variable.name) + variable.values.capacity() * sizeof(double);
    for (const auto &bin : model.histogramBins)
        bytes += bin.rows.capacity() * sizeof(int);
    for (const auto &bin : model.barplotBins) {
        bytes += StringBytes(bin.category) + bin.rows.capacity() * sizeof(int);
        for (const auto &segment : bin.segments)
            bytes += sizeof(segment) + StringBytes(segment.level) + segment.rows.capacity() * sizeof(int);
    }
    for (const auto &curve : model.smoothCurves)
        bytes += curve.x.capacity() * sizeof(double) + curve.y.capacity() * sizeof(double);
    return bytes;
}

} // namespace

std::string GenerateSnapshotId()
{
    std::array<unsigned char, 16> bytes{};
    std::random_device random;
    for (unsigned char &byte : bytes) byte = static_cast<unsigned char>(random());
    bytes[6] = static_cast<unsigned char>((bytes[6] & 0x0fU) | 0x40U);
    bytes[8] = static_cast<unsigned char>((bytes[8] & 0x3fU) | 0x80U);
    std::ostringstream out;
    out << "snapshot-" << std::hex << std::setfill('0');
    for (std::size_t index = 0; index < bytes.size(); ++index) {
        if (index == 4 || index == 6 || index == 8 || index == 10) out << '-';
        out << std::setw(2) << static_cast<unsigned int>(bytes[index]);
    }
    return out.str();
}

std::string SnapshotContentKindName(SnapshotContentKind kind)
{
    switch (kind) {
    case SnapshotContentKind::Plot: return "Plot";
    case SnapshotContentKind::StatisticalTable: return "Statistical Table";
    case SnapshotContentKind::DataTable: return "Data Table";
    case SnapshotContentKind::Trellis: return "Trellis";
    case SnapshotContentKind::ModelResult: return "Model Result";
    case SnapshotContentKind::OtherRenderable: return "Renderable Result";
    }
    return "Renderable Result";
}

std::string SnapshotScopeSummary(const SnapshotItem &item)
{
    const std::size_t totalRows = item.metadata.source_scope.totalDatasetRows;
    return AnalysisScopeSummary(item.metadata.source_scope, totalRows, true);
}

std::size_t EstimateSnapshotMemoryBytes(const SnapshotItem &item)
{
    std::size_t bytes = sizeof(item) + StringBytes(item.metadata.snapshot_id) +
        StringBytes(item.metadata.source_view_id) + StringBytes(item.metadata.dataset_id) +
        StringBytes(item.metadata.title) + StringBytes(item.metadata.default_title) +
        StringBytes(item.metadata.source_description) + StringBytes(item.metadata.theme_name);
    bytes += item.metadata.source_scope.originalRowIds.capacity() * sizeof(int);
    if (const auto *plot = std::get_if<FrozenPlotContent>(&item.renderable_content)) {
        bytes += PlotMemoryBytes(plot->model);
    } else if (const auto *table = std::get_if<FrozenTableContent>(&item.renderable_content)) {
        bytes += StringBytes(table->tab_delimited_text) +
            table->column_widths.capacity() * sizeof(double);
    } else if (const auto *image = std::get_if<FrozenImageContent>(&item.renderable_content)) {
        bytes += image->png_data.capacity() * sizeof(unsigned char);
    }
    if (item.note) bytes += sizeof(WindowNote) + StringBytes(item.note->plain_text);
    bytes += item.stickers.capacity() * sizeof(WindowStickyNote);
    for (const WindowStickyNote &sticker : item.stickers)
        bytes += StringBytes(sticker.note_id) + StringBytes(sticker.plain_text);
    return bytes;
}

const std::vector<SnapshotItem> &SnapshotAlbum::items() const { return items_; }
std::size_t SnapshotAlbum::size() const { return items_.size(); }
bool SnapshotAlbum::empty() const { return items_.empty(); }

SnapshotItem &SnapshotAlbum::add(SnapshotItem item)
{
    if (item.metadata.snapshot_id.empty() || find(item.metadata.snapshot_id))
        item.metadata.snapshot_id = GenerateSnapshotId();
    if (!HasNonWhitespace(item.metadata.default_title))
        item.metadata.default_title = SnapshotContentKindName(item.content_kind);
    if (!HasNonWhitespace(item.metadata.title)) item.metadata.title = item.metadata.default_title;
    if (item.metadata.created_at == std::chrono::system_clock::time_point{})
        item.metadata.created_at = std::chrono::system_clock::now();
    if (item.note) {
        item.note->view_id = item.metadata.snapshot_id;
        item.note->include_in_export = item.metadata.include_notes && item.note->has_content;
    }
    items_.push_back(std::move(item));
    return items_.back();
}

SnapshotItem *SnapshotAlbum::find(const std::string &snapshotId)
{
    auto found = std::find_if(items_.begin(), items_.end(), [&](const SnapshotItem &item) {
        return item.metadata.snapshot_id == snapshotId;
    });
    return found == items_.end() ? nullptr : &*found;
}

const SnapshotItem *SnapshotAlbum::find(const std::string &snapshotId) const
{
    auto found = std::find_if(items_.begin(), items_.end(), [&](const SnapshotItem &item) {
        return item.metadata.snapshot_id == snapshotId;
    });
    return found == items_.end() ? nullptr : &*found;
}

bool SnapshotAlbum::remove(const std::string &snapshotId)
{
    auto found = std::find_if(items_.begin(), items_.end(), [&](const SnapshotItem &item) {
        return item.metadata.snapshot_id == snapshotId;
    });
    if (found == items_.end()) return false;
    items_.erase(found);
    return true;
}

bool SnapshotAlbum::rename(const std::string &snapshotId, const std::string &title)
{
    SnapshotItem *item = find(snapshotId);
    if (!item) return false;
    item->metadata.title = HasNonWhitespace(title) ? title : item->metadata.default_title;
    return true;
}

bool SnapshotAlbum::move(const std::string &snapshotId, std::size_t destinationIndex)
{
    auto found = std::find_if(items_.begin(), items_.end(), [&](const SnapshotItem &item) {
        return item.metadata.snapshot_id == snapshotId;
    });
    if (found == items_.end() || items_.empty()) return false;
    const std::size_t sourceIndex = static_cast<std::size_t>(std::distance(items_.begin(), found));
    destinationIndex = std::min(destinationIndex, items_.size() - 1);
    if (sourceIndex == destinationIndex) return true;
    SnapshotItem item = std::move(*found);
    items_.erase(found);
    items_.insert(items_.begin() + static_cast<std::ptrdiff_t>(destinationIndex), std::move(item));
    return true;
}

bool SnapshotAlbum::moveUp(const std::string &snapshotId)
{
    for (std::size_t index = 0; index < items_.size(); ++index) {
        if (items_[index].metadata.snapshot_id == snapshotId)
            return index > 0 && move(snapshotId, index - 1);
    }
    return false;
}

bool SnapshotAlbum::moveDown(const std::string &snapshotId)
{
    for (std::size_t index = 0; index < items_.size(); ++index) {
        if (items_[index].metadata.snapshot_id == snapshotId)
            return index + 1 < items_.size() && move(snapshotId, index + 1);
    }
    return false;
}

bool SnapshotAlbum::setIncludedInExport(const std::string &snapshotId, bool included)
{
    SnapshotItem *item = find(snapshotId);
    if (!item) return false;
    item->metadata.include_in_export = included;
    return true;
}

bool SnapshotAlbum::setNotesIncluded(const std::string &snapshotId, bool included)
{
    SnapshotItem *item = find(snapshotId);
    if (!item) return false;
    item->metadata.include_notes = included;
    if (item->note) item->note->include_in_export = included && item->note->has_content;
    return true;
}

bool SnapshotAlbum::setNoteVisible(const std::string &snapshotId, bool visible)
{
    SnapshotItem *item = find(snapshotId);
    if (!item) return false;
    if (!item->note) {
        if (!visible) return true;
        item->note = MakeWindowNote(snapshotId);
    }
    item->note->visible = visible;
    return true;
}

bool SnapshotAlbum::setNoteText(const std::string &snapshotId, const std::string &text,
                                WindowNoteTimePoint now)
{
    SnapshotItem *item = find(snapshotId);
    if (!item) return false;
    if (!item->note) item->note = MakeWindowNote(snapshotId);
    SetWindowNoteText(*item->note, text, now);
    item->note->include_in_export = item->metadata.include_notes && item->note->has_content;
    return true;
}

std::vector<const SnapshotItem *> SnapshotAlbum::exportItems() const
{
    std::vector<const SnapshotItem *> result;
    for (const SnapshotItem &item : items_)
        if (item.metadata.include_in_export) result.push_back(&item);
    return result;
}

void SnapshotAlbum::clear() { items_.clear(); }

std::string SnapshotAlbumManifestCSV(const SnapshotAlbum &album)
{
    std::ostringstream out;
    out << "order,snapshot_id,title,type,source,dataset,scope,theme,include_notes\n";
    std::size_t order = 0;
    for (const SnapshotItem *item : album.exportItems()) {
        ++order;
        out << order << ',' << CsvCell(item->metadata.snapshot_id) << ','
            << CsvCell(item->metadata.title) << ','
            << CsvCell(SnapshotContentKindName(item->content_kind)) << ','
            << CsvCell(item->metadata.source_description) << ','
            << CsvCell(item->metadata.dataset_id) << ','
            << CsvCell(SnapshotScopeSummary(*item)) << ','
            << CsvCell(item->metadata.theme_name) << ','
            << (item->metadata.include_notes &&
                ((item->note && item->note->has_content) || !item->stickers.empty())
                    ? "true" : "false")
            << '\n';
    }
    return out.str();
}

} // namespace rlispstat::core
