#include "linkeda_document.h"

#include <algorithm>
#include <chrono>
#include <cstring>
#include <limits>
#include <set>
#include <sstream>

namespace rlispstat::core {
namespace {

constexpr char kPayloadMagic[] = "LINKEDA-DOCUMENT-PAYLOAD-V1";
constexpr std::uint64_t kMaximumCollectionSize = 100000000ULL;

class Writer {
public:
    void u8(std::uint8_t value) { bytes_.push_back(value); }
    void u32(std::uint32_t value) {
        for (int shift = 0; shift < 32; shift += 8)
            bytes_.push_back(static_cast<unsigned char>((value >> shift) & 0xffU));
    }
    void u64(std::uint64_t value) {
        for (int shift = 0; shift < 64; shift += 8)
            bytes_.push_back(static_cast<unsigned char>((value >> shift) & 0xffU));
    }
    void i64(std::int64_t value) { u64(static_cast<std::uint64_t>(value)); }
    void f64(double value) {
        std::uint64_t bits = 0; std::memcpy(&bits, &value, sizeof(value)); u64(bits);
    }
    void string(const std::string &value) {
        u64(value.size());
        bytes_.insert(bytes_.end(), value.begin(), value.end());
    }
    void strings(const std::vector<std::string> &values) {
        u64(values.size());
        for (const auto &value : values) string(value);
    }
    void bools(const std::vector<bool> &values) {
        u64(values.size());
        for (bool value : values) u8(value ? 1 : 0);
    }
    void ints(const std::vector<int> &values) {
        u64(values.size());
        for (int value : values) i64(value);
    }
    void optionalString(const std::optional<std::string> &value) {
        u8(value ? 1 : 0);
        if (value) string(*value);
    }
    std::vector<unsigned char> take() { return std::move(bytes_); }
private:
    std::vector<unsigned char> bytes_;
};

class Reader {
public:
    explicit Reader(const std::vector<unsigned char> &bytes) : bytes_(bytes) {}
    bool u8(std::uint8_t &value) {
        if (position_ >= bytes_.size()) return fail("Unexpected end of document.");
        value = bytes_[position_++]; return true;
    }
    bool u32(std::uint32_t &value) {
        std::uint64_t wide = 0; if (!number(wide, 4)) return false;
        value = static_cast<std::uint32_t>(wide); return true;
    }
    bool u64(std::uint64_t &value) { return number(value, 8); }
    bool i64(std::int64_t &value) {
        std::uint64_t wide = 0; if (!u64(wide)) return false;
        value = static_cast<std::int64_t>(wide); return true;
    }
    bool f64(double &value) {
        std::uint64_t bits = 0; if (!u64(bits)) return false;
        std::memcpy(&value, &bits, sizeof(value)); return true;
    }
    bool string(std::string &value) {
        std::uint64_t size = 0;
        if (!u64(size) || size > bytes_.size() - position_)
            return fail("A document string is truncated.");
        value.assign(reinterpret_cast<const char *>(bytes_.data() + position_),
                     static_cast<std::size_t>(size));
        position_ += static_cast<std::size_t>(size); return true;
    }
    bool strings(std::vector<std::string> &values) {
        std::uint64_t size = 0; if (!count(size)) return false;
        values.clear(); values.reserve(static_cast<std::size_t>(size));
        for (std::uint64_t i = 0; i < size; ++i) {
            std::string value; if (!string(value)) return false;
            values.push_back(std::move(value));
        }
        return true;
    }
    bool bools(std::vector<bool> &values) {
        std::uint64_t size = 0; if (!count(size)) return false;
        values.clear(); values.reserve(static_cast<std::size_t>(size));
        for (std::uint64_t i = 0; i < size; ++i) {
            std::uint8_t value = 0; if (!u8(value) || value > 1) return fail("Invalid boolean value.");
            values.push_back(value != 0);
        }
        return true;
    }
    bool ints(std::vector<int> &values) {
        std::uint64_t size = 0; if (!count(size)) return false;
        values.clear(); values.reserve(static_cast<std::size_t>(size));
        for (std::uint64_t i = 0; i < size; ++i) {
            std::int64_t value = 0; if (!i64(value)) return false;
            if (value < std::numeric_limits<int>::min() || value > std::numeric_limits<int>::max())
                return fail("Integer value is outside the supported range.");
            values.push_back(static_cast<int>(value));
        }
        return true;
    }
    bool optionalString(std::optional<std::string> &value) {
        std::uint8_t present = 0; if (!u8(present) || present > 1) return fail("Invalid optional value.");
        if (!present) { value.reset(); return true; }
        std::string text; if (!string(text)) return false;
        value = std::move(text); return true;
    }
    bool count(std::uint64_t &value) {
        if (!u64(value)) return false;
        return value <= kMaximumCollectionSize ? true : fail("Document collection is unreasonably large.");
    }
    bool done() const { return position_ == bytes_.size(); }
    const std::string &error() const { return error_; }
private:
    bool number(std::uint64_t &value, std::size_t width) {
        if (width > bytes_.size() - position_) return fail("Unexpected end of document.");
        value = 0;
        for (std::size_t i = 0; i < width; ++i)
            value |= static_cast<std::uint64_t>(bytes_[position_++]) << (8 * i);
        return true;
    }
    bool fail(const std::string &message) { if (error_.empty()) error_ = message; return false; }
    const std::vector<unsigned char> &bytes_;
    std::size_t position_ = 0;
    std::string error_;
};

std::int64_t TimeValue(WindowNoteTimePoint value)
{
    return std::chrono::duration_cast<std::chrono::milliseconds>(
        value.time_since_epoch()).count();
}

WindowNoteTimePoint TimePoint(std::int64_t value)
{
    return WindowNoteTimePoint(std::chrono::milliseconds(value));
}

void WriteScope(Writer &out, const AnalysisScope &scope)
{
    out.u32(static_cast<std::uint32_t>(scope.kind));
    out.string(scope.datasetId); out.ints(scope.originalRowIds);
    out.u32(static_cast<std::uint32_t>(scope.sourceKind));
    out.string(scope.sourceDescription); out.optionalString(scope.sourceViewId);
    out.optionalString(scope.sourceElementId); out.u64(scope.totalDatasetRows);
    out.u64(scope.invalidatedRowCount);
}

bool ReadScope(Reader &in, AnalysisScope &scope)
{
    std::uint32_t kind = 0, source = 0;
    std::uint64_t total = 0, invalid = 0;
    if (!in.u32(kind) || !in.string(scope.datasetId) || !in.ints(scope.originalRowIds) ||
        !in.u32(source) || !in.string(scope.sourceDescription) ||
        !in.optionalString(scope.sourceViewId) || !in.optionalString(scope.sourceElementId) ||
        !in.u64(total) || !in.u64(invalid)) return false;
    if (kind > static_cast<std::uint32_t>(AnalysisScopeKind::ExplicitRowIds) ||
        source > static_cast<std::uint32_t>(AnalysisScopeSourceKind::IncludedObservations)) return false;
    scope.kind = static_cast<AnalysisScopeKind>(kind);
    scope.sourceKind = static_cast<AnalysisScopeSourceKind>(source);
    scope.totalDatasetRows = static_cast<std::size_t>(total);
    scope.invalidatedRowCount = static_cast<std::size_t>(invalid);
    return true;
}

void WriteColumn(Writer &out, const DataColumn &column,
                 std::uint32_t documentVersion)
{
    out.string(column.name); out.string(column.type); out.string(column.displayName);
    out.string(column.description); out.i64(column.decimals);
    out.strings(column.values); out.strings(column.displayValues); out.strings(column.definedLevels);
    out.bools(column.imputedMissing); out.strings(column.imputationOriginalValues);
    out.u64(column.imputationValues.size());
    for (const auto &values : column.imputationValues) out.strings(values);
    out.u64(column.imputationOriginalSparse.size());
    for (const auto &[row, value] : column.imputationOriginalSparse) { out.u64(row); out.string(value); }
    out.u64(column.imputationValuesSparse.size());
    for (const auto &values : column.imputationValuesSparse) {
        out.u64(values.size());
        for (const auto &[row, value] : values) { out.u64(row); out.string(value); }
    }
    if (documentVersion >= 6) out.strings(column.reversibleFactorLevels);
    if (documentVersion >= 7) {
        out.string(column.storageType);
        out.u64(column.numericMapping.size());
        for (const auto &[label, value] : column.numericMapping) {
            out.string(label); out.string(value);
        }
        out.string(column.reversibleCategoryType);
        out.u8(column.binary ? 1 : 0);
    }
}

bool ReadColumn(Reader &in, DataColumn &column,
                std::uint32_t documentVersion)
{
    std::int64_t decimals = -1;
    if (!in.string(column.name) || !in.string(column.type) || !in.string(column.displayName) ||
        !in.string(column.description) || !in.i64(decimals) || !in.strings(column.values) ||
        !in.strings(column.displayValues) || !in.strings(column.definedLevels) ||
        !in.bools(column.imputedMissing) || !in.strings(column.imputationOriginalValues)) return false;
    column.decimals = static_cast<int>(decimals);
    std::uint64_t size = 0;
    if (!in.count(size)) return false;
    column.imputationValues.clear(); column.imputationValues.resize(static_cast<std::size_t>(size));
    for (auto &values : column.imputationValues) if (!in.strings(values)) return false;
    if (!in.count(size)) return false;
    column.imputationOriginalSparse.clear();
    for (std::uint64_t i = 0; i < size; ++i) {
        std::uint64_t row = 0; std::string value;
        if (!in.u64(row) || !in.string(value)) return false;
        column.imputationOriginalSparse[static_cast<std::size_t>(row)] = std::move(value);
    }
    if (!in.count(size)) return false;
    column.imputationValuesSparse.clear(); column.imputationValuesSparse.resize(static_cast<std::size_t>(size));
    for (auto &values : column.imputationValuesSparse) {
        std::uint64_t entries = 0; if (!in.count(entries)) return false;
        for (std::uint64_t i = 0; i < entries; ++i) {
            std::uint64_t row = 0; std::string value;
            if (!in.u64(row) || !in.string(value)) return false;
            values[static_cast<std::size_t>(row)] = std::move(value);
        }
    }
    if (documentVersion >= 6 && !in.strings(column.reversibleFactorLevels))
        return false;
    if (documentVersion >= 7) {
        std::uint64_t mappings = 0;
        std::uint8_t binary = 0;
        if (!in.string(column.storageType) || !in.count(mappings)) return false;
        for (std::uint64_t index = 0; index < mappings; ++index) {
            std::string label, value;
            if (!in.string(label) || !in.string(value)) return false;
            column.numericMapping[std::move(label)] = std::move(value);
        }
        if (!in.string(column.reversibleCategoryType) || !in.u8(binary) || binary > 1)
            return false;
        column.binary = binary != 0;
    } else {
        column.storageType = DataColumnStorageType(column);
        column.binary = DataColumnIsBinaryCategorical(column);
        if (!column.reversibleFactorLevels.empty()) {
            column.reversibleCategoryType = "factor";
            if (column.reversibleFactorLevels.size() == 2) {
                column.numericMapping[column.reversibleFactorLevels[0]] = "0";
                column.numericMapping[column.reversibleFactorLevels[1]] = "1";
            }
        }
    }
    return true;
}

void WriteStringMap(Writer &out, const std::map<std::string, std::string> &values)
{
    out.u64(values.size());
    for (const auto &[key, value] : values) { out.string(key); out.string(value); }
}

bool ReadStringMap(Reader &in, std::map<std::string, std::string> &values)
{
    std::uint64_t size = 0; if (!in.count(size)) return false;
    values.clear();
    for (std::uint64_t i = 0; i < size; ++i) {
        std::string key, value; if (!in.string(key) || !in.string(value)) return false;
        values[std::move(key)] = std::move(value);
    }
    return true;
}

void WriteVersionReference(Writer &out, const DataVersionReference &value)
{
    out.string(value.datasetId); out.string(value.displayName); out.u64(value.version);
    out.string(value.objectType); out.i64(value.imputationCount); out.string(value.storageKey);
}

bool ReadVersionReference(Reader &in, DataVersionReference &value)
{
    std::int64_t count = 0;
    if (!in.string(value.datasetId) || !in.string(value.displayName) || !in.u64(value.version) ||
        !in.string(value.objectType) || !in.i64(count) || !in.string(value.storageKey)) return false;
    value.imputationCount = static_cast<int>(count); return true;
}

void WriteTransformation(Writer &out, const TransformationStep &step)
{
    out.string(step.id); out.string(step.label); out.u32(static_cast<std::uint32_t>(step.origin));
    out.string(step.rCode); out.strings(step.parentVersionKeys); out.strings(step.inputColumns);
    out.strings(step.outputColumns); out.strings(step.stableRowIds);
    WriteStringMap(out, step.parameters); WriteStringMap(out, step.packageVersions);
    out.u8(step.seed ? 1 : 0); if (step.seed) out.u64(*step.seed);
}

bool ReadTransformation(Reader &in, TransformationStep &step)
{
    std::uint32_t origin = 0; std::uint8_t hasSeed = 0;
    if (!in.string(step.id) || !in.string(step.label) || !in.u32(origin) || origin > 2 ||
        !in.string(step.rCode) || !in.strings(step.parentVersionKeys) ||
        !in.strings(step.inputColumns) || !in.strings(step.outputColumns) ||
        !in.strings(step.stableRowIds) || !ReadStringMap(in, step.parameters) ||
        !ReadStringMap(in, step.packageVersions) || !in.u8(hasSeed) || hasSeed > 1) return false;
    step.origin = static_cast<RCodeOrigin>(origin);
    if (hasSeed) { std::uint64_t seed = 0; if (!in.u64(seed)) return false; step.seed = seed; }
    return true;
}

void WriteDataProvenance(Writer &out, const DataProvenance &value)
{
    WriteVersionReference(out, value.currentVersion);
    out.u32(static_cast<std::uint32_t>(value.origin)); out.string(value.originCode);
    out.string(value.originDescription); out.u64(value.history.size());
    for (const auto &step : value.history) WriteTransformation(out, step);
    out.u64(value.columnSteps.size());
    for (const auto &[column, steps] : value.columnSteps) { out.string(column); out.strings(steps); }
}

bool ReadDataProvenance(Reader &in, DataProvenance &value)
{
    std::uint32_t origin = 0; std::uint64_t size = 0;
    if (!ReadVersionReference(in, value.currentVersion) || !in.u32(origin) || origin > 2 ||
        !in.string(value.originCode) || !in.string(value.originDescription) || !in.count(size)) return false;
    value.origin = static_cast<RCodeOrigin>(origin); value.history.resize(static_cast<std::size_t>(size));
    for (auto &step : value.history) if (!ReadTransformation(in, step)) return false;
    if (!in.count(size)) return false;
    for (std::uint64_t i = 0; i < size; ++i) {
        std::string column; std::vector<std::string> steps;
        if (!in.string(column) || !in.strings(steps)) return false;
        value.columnSteps[std::move(column)] = std::move(steps);
    }
    return true;
}

void WriteSharedDataFrame(Writer &out, const DataFrameModel &data,
                          std::uint32_t documentVersion)
{
    out.string(data.group); out.i64(data.rows); out.string(data.datasetType);
    out.string(data.imputationId); out.string(data.sourceDatasetId);
    out.i64(data.imputationCount); out.i64(data.activeImputationVersion);
    out.string(data.imputationDisplayMode);
    out.u64(data.columns.size());
    for (const DataColumn &column : data.columns)
        WriteColumn(out, column, documentVersion);
    out.strings(data.stableRowIds); out.u64(data.dataVersion);
    WriteDataProvenance(out, data.provenance);
    if (documentVersion >= 8) out.string(data.imputationProcess);
}

bool ReadSharedDataFrame(Reader &in, DataFrameModel &data,
                         std::uint32_t documentVersion)
{
    std::int64_t rows = 0, imputationCount = 0, activeImputation = 0;
    std::uint64_t columnCount = 0, dataVersion = 0;
    if (!in.string(data.group) || !in.i64(rows) || !in.string(data.datasetType) ||
        !in.string(data.imputationId) || !in.string(data.sourceDatasetId) ||
        !in.i64(imputationCount) || !in.i64(activeImputation) ||
        !in.string(data.imputationDisplayMode) || !in.count(columnCount)) return false;
    data.rows = static_cast<int>(rows);
    data.imputationCount = static_cast<int>(imputationCount);
    data.activeImputationVersion = static_cast<int>(activeImputation);
    data.columns.resize(static_cast<std::size_t>(columnCount));
    for (DataColumn &column : data.columns)
        if (!ReadColumn(in, column, documentVersion)) return false;
    if (!in.strings(data.stableRowIds) || !in.u64(dataVersion) ||
        !ReadDataProvenance(in, data.provenance)) return false;
    data.dataVersion = dataVersion;
    if (documentVersion >= 8 && !in.string(data.imputationProcess)) return false;
    return true;
}

void WriteImmutableScope(Writer &out, const ImmutableAnalysisScope &value)
{
    out.string(value.kind); out.string(value.sourceKind); out.string(value.description);
    out.strings(value.requestedStableRowIds); out.strings(value.effectiveStableRowIds);
    out.strings(value.excludedStableRowIds); out.u64(value.sourceN); out.u64(value.scopeN);
    out.u64(value.effectiveN); out.strings(value.warnings);
}

bool ReadImmutableScope(Reader &in, ImmutableAnalysisScope &value)
{
    std::uint64_t source = 0, scope = 0, effective = 0;
    if (!in.string(value.kind) || !in.string(value.sourceKind) || !in.string(value.description) ||
        !in.strings(value.requestedStableRowIds) || !in.strings(value.effectiveStableRowIds) ||
        !in.strings(value.excludedStableRowIds) || !in.u64(source) || !in.u64(scope) ||
        !in.u64(effective) || !in.strings(value.warnings)) return false;
    value.sourceN = static_cast<std::size_t>(source); value.scopeN = static_cast<std::size_t>(scope);
    value.effectiveN = static_cast<std::size_t>(effective); return true;
}

void WriteAnalysisProvenance(Writer &out, const AnalysisProvenance &value)
{
    out.string(value.analysisId); out.string(value.title); WriteVersionReference(out, value.dataVersion);
    WriteImmutableScope(out, value.scope); out.u32(static_cast<std::uint32_t>(value.codeOrigin));
    out.string(value.executedRCode); WriteStringMap(out, value.outputRCode);
    WriteStringMap(out, value.packageVersions); out.strings(value.warnings);
}

bool ReadAnalysisProvenance(Reader &in, AnalysisProvenance &value)
{
    std::uint32_t origin = 0;
    if (!in.string(value.analysisId) || !in.string(value.title) ||
        !ReadVersionReference(in, value.dataVersion) || !ReadImmutableScope(in, value.scope) ||
        !in.u32(origin) || origin > 2 || !in.string(value.executedRCode) ||
        !ReadStringMap(in, value.outputRCode) || !ReadStringMap(in, value.packageVersions) ||
        !in.strings(value.warnings)) return false;
    value.codeOrigin = static_cast<RCodeOrigin>(origin); return true;
}

void WritePublicationValue(Writer &out, const PublicationValue &value)
{
    out.u32(static_cast<std::uint32_t>(value.kind)); out.f64(value.number);
    out.string(value.text); out.u8(value.logical ? 1 : 0);
}

bool ReadPublicationValue(Reader &in, PublicationValue &value)
{
    std::uint32_t kind = 0; std::uint8_t logical = 0;
    if (!in.u32(kind) || kind > 3 || !in.f64(value.number) || !in.string(value.text) ||
        !in.u8(logical) || logical > 1) return false;
    value.kind = static_cast<PublicationValueKind>(kind); value.logical = logical != 0; return true;
}

void WriteTableSpec(Writer &out, const PublicationTableSpec &value)
{
    out.string(value.title); out.string(value.subtitle); out.u64(value.columns.size());
    for (const auto &column : value.columns) {
        out.string(column.key); out.string(column.label); out.string(column.spanner);
        out.i64(column.decimals); out.u8(column.pValue ? 1 : 0);
    }
    out.u64(value.rows.size());
    for (const auto &row : value.rows) { out.u64(row.size()); for (const auto &cell : row) WritePublicationValue(out, cell); }
    out.strings(value.footnotes); out.u64(value.stubColumns.size());
    for (std::size_t column : value.stubColumns) out.u64(column);
}

bool ReadTableSpec(Reader &in, PublicationTableSpec &value)
{
    std::uint64_t size = 0;
    if (!in.string(value.title) || !in.string(value.subtitle) || !in.count(size)) return false;
    value.columns.resize(static_cast<std::size_t>(size));
    for (auto &column : value.columns) {
        std::int64_t decimals = 0; std::uint8_t p = 0;
        if (!in.string(column.key) || !in.string(column.label) || !in.string(column.spanner) ||
            !in.i64(decimals) || !in.u8(p) || p > 1) return false;
        column.decimals = static_cast<int>(decimals); column.pValue = p != 0;
    }
    if (!in.count(size)) return false; value.rows.resize(static_cast<std::size_t>(size));
    for (auto &row : value.rows) {
        std::uint64_t cells = 0; if (!in.count(cells)) return false;
        row.resize(static_cast<std::size_t>(cells));
        for (auto &cell : row) if (!ReadPublicationValue(in, cell)) return false;
    }
    if (!in.strings(value.footnotes) || !in.count(size)) return false;
    value.stubColumns.resize(static_cast<std::size_t>(size));
    for (std::size_t &column : value.stubColumns) { std::uint64_t v = 0; if (!in.u64(v)) return false; column = static_cast<std::size_t>(v); }
    return true;
}

void WritePlotSpec(Writer &out, const PublicationPlotSpec &value,
                   std::uint32_t documentVersion)
{
    out.string(value.kind); out.string(value.title); out.string(value.subtitle);
    out.string(value.xLabel); out.string(value.yLabel); out.strings(value.xCategoryOrder);
    out.u64(value.series.size());
    for (const auto &series : value.series) {
        out.string(series.id); out.string(series.label);
        for (const auto *numbers : {&series.x, &series.y, &series.lower, &series.upper}) {
            out.u64(numbers->size()); for (double number : *numbers) out.f64(number);
        }
    }
    out.u8(value.showPoints); out.u8(value.showLines); out.u8(value.showConfidenceIntervals);
    if (documentVersion >= 4) {
        out.u8(value.showAxisTickMarks); out.u8(value.showAxisTickLabels);
    }
    out.f64(value.confidenceLevel);
    if (documentVersion >= 5) out.string(value.theme);
}

bool ReadPlotSpec(Reader &in, PublicationPlotSpec &value,
                  std::uint32_t documentVersion)
{
    std::uint64_t size = 0;
    if (!in.string(value.kind) || !in.string(value.title) || !in.string(value.subtitle) ||
        !in.string(value.xLabel) || !in.string(value.yLabel) || !in.strings(value.xCategoryOrder) ||
        !in.count(size)) return false;
    value.series.resize(static_cast<std::size_t>(size));
    for (auto &series : value.series) {
        if (!in.string(series.id) || !in.string(series.label)) return false;
        for (auto *numbers : {&series.x, &series.y, &series.lower, &series.upper}) {
            std::uint64_t count = 0; if (!in.count(count)) return false;
            numbers->resize(static_cast<std::size_t>(count));
            for (double &number : *numbers) if (!in.f64(number)) return false;
        }
    }
    std::uint8_t points = 0, lines = 0, ci = 0;
    if (!in.u8(points) || !in.u8(lines) || !in.u8(ci) || points > 1 || lines > 1 || ci > 1)
        return false;
    if (documentVersion >= 4) {
        std::uint8_t tickMarks = 0, tickLabels = 0;
        if (!in.u8(tickMarks) || !in.u8(tickLabels) || tickMarks > 1 || tickLabels > 1)
            return false;
        value.showAxisTickMarks = tickMarks != 0;
        value.showAxisTickLabels = tickLabels != 0;
    }
    if (!in.f64(value.confidenceLevel)) return false;
    if (documentVersion >= 5 && !in.string(value.theme)) return false;
    value.showPoints = points != 0; value.showLines = lines != 0; value.showConfidenceIntervals = ci != 0;
    return true;
}

void WriteOutputReference(Writer &out, const OutputCodeReference &value,
                          std::uint32_t documentVersion)
{
    out.string(value.outputId); out.string(value.analysisId); out.string(value.outputBlockId);
    out.string(value.title); out.string(value.kind); WriteAnalysisProvenance(out, value.provenance);
    out.u64(value.publication.availableBackends.size());
    for (auto backend : value.publication.availableBackends) out.u32(static_cast<std::uint32_t>(backend));
    out.u8(value.publication.table ? 1 : 0); if (value.publication.table) WriteTableSpec(out, *value.publication.table);
    out.u8(value.publication.plot ? 1 : 0); if (value.publication.plot) WritePlotSpec(out, *value.publication.plot, documentVersion);
    if (documentVersion >= 3) {
        WriteStringMap(out, value.provenance.verificationRCode);
        out.strings(value.provenance.verificationVariables);
        out.strings(value.provenance.verificationWarnings);
    }
}

bool ReadOutputReference(Reader &in, OutputCodeReference &value,
                         std::uint32_t documentVersion)
{
    std::uint64_t size = 0;
    if (!in.string(value.outputId) || !in.string(value.analysisId) || !in.string(value.outputBlockId) ||
        !in.string(value.title) || !in.string(value.kind) || !ReadAnalysisProvenance(in, value.provenance) ||
        !in.count(size)) return false;
    value.publication.availableBackends.resize(static_cast<std::size_t>(size));
    for (auto &backend : value.publication.availableBackends) {
        std::uint32_t raw = 0; if (!in.u32(raw) || raw > 3) return false;
        backend = static_cast<PublicationBackend>(raw);
    }
    std::uint8_t present = 0;
    if (!in.u8(present) || present > 1) return false;
    if (present) { PublicationTableSpec table; if (!ReadTableSpec(in, table)) return false; value.publication.table = std::move(table); }
    if (!in.u8(present) || present > 1) return false;
    if (present) { PublicationPlotSpec plot; if (!ReadPlotSpec(in, plot, documentVersion)) return false; value.publication.plot = std::move(plot); }
    if (documentVersion >= 3 &&
        (!ReadStringMap(in, value.provenance.verificationRCode) ||
         !in.strings(value.provenance.verificationVariables) ||
         !in.strings(value.provenance.verificationWarnings))) return false;
    return true;
}

bool ValidVectorSize(std::size_t size, int rows) { return size == 0 || size == static_cast<std::size_t>(rows); }

} // namespace

bool ValidateLinkEDADataDocument(const LinkEDADataDocument &document, std::string *error)
{
    const auto fail = [error](const std::string &message) { if (error) *error = message; return false; };
    if (document.format != kLinkEDADataDocumentFormat) return fail("This is not a LinkEDA data document.");
    if (document.version > kLinkEDADataDocumentVersion) return fail("This LinkEDA document was created by a newer unsupported version.");
    if (document.version < 1) return fail("Unsupported LinkEDA document version.");
    if (document.dataset.group.empty()) return fail("The document dataset has no name.");
    if (document.dataset.rows < 0) return fail("The document has an invalid row count.");
    std::set<std::string> names;
    for (const DataColumn &column : document.dataset.columns) {
        if (column.name.empty() || !names.insert(column.name).second)
            return fail("Document variable names must be non-empty and unique.");
        if (!ValidVectorSize(column.values.size(), document.dataset.rows) ||
            !ValidVectorSize(column.displayValues.size(), document.dataset.rows) ||
            !ValidVectorSize(column.imputedMissing.size(), document.dataset.rows) ||
            !ValidVectorSize(column.imputationOriginalValues.size(), document.dataset.rows))
            return fail("Variable `" + column.name + "` has an inconsistent number of rows.");
        for (const auto &values : column.imputationValues)
            if (!ValidVectorSize(values.size(), document.dataset.rows))
                return fail("Variable `" + column.name + "` has inconsistent imputation data.");
    }
    if (document.version >= 2) {
        if (document.dataset.stableRowIds.size() != static_cast<std::size_t>(document.dataset.rows))
            return fail("The document has inconsistent stable row identities.");
        if (!ValidateDataProvenance(document.dataset.provenance, error)) return false;
        for (const OutputCodeReference &reference : document.output_code_references) {
            if (!ValidateAnalysisProvenance(reference.provenance, error) ||
                !ValidatePublicationSpec(reference.publication, error)) return false;
        }
    }
    if (document.version >= 3) {
        std::set<std::string> versionKeys;
        for (const DataFrameModel &version : document.shared_data_versions) {
            if (version.group != document.dataset.group)
                return fail("A shared data version belongs to another dataset.");
            if (version.rows < 0 || version.stableRowIds.size() !=
                    static_cast<std::size_t>(version.rows) || version.dataVersion == 0)
                return fail("A shared historical data version is invalid.");
            const std::string key = version.provenance.currentVersion.storageKey.empty()
                ? version.group + "@" + std::to_string(version.dataVersion)
                : version.provenance.currentVersion.storageKey;
            if (!versionKeys.insert(key).second)
                return fail("A shared historical data version is duplicated.");
        }
    }
    for (const auto &[name, role] : document.variable_roles) {
        if (!names.count(name)) return fail("A variable role refers to unknown variable `" + name + "`.");
        if (role != "none" && role != "dependent" && role != "independent")
            return fail("Variable `" + name + "` has an unsupported role.");
    }
    for (const auto &[row, color] : document.row_colors)
        if (row <= 0 || row > document.dataset.rows || color.empty())
            return fail("The document contains an invalid row colour.");
    if (NormalizeAnalysisScopeRowIds(document.excluded_rows).size() !=
        document.excluded_rows.size())
        return fail("The document has duplicate or invalid excluded cases.");
    for (int row : document.excluded_rows)
        if (row > document.dataset.rows)
            return fail("The document has an excluded case outside the dataset.");
    if (!document.point_label_column.empty() && !names.count(document.point_label_column))
        return fail("The point-label variable does not exist in the dataset.");
    AnalysisScope scope = document.analysis_scope;
    scope.datasetId = document.dataset.group;
    if (!ValidateAnalysisScope(scope, document.dataset.group,
                               static_cast<std::size_t>(document.dataset.rows), error)) return false;
    std::set<std::string> selectionNames;
    for (const SavedSelection &selection : document.saved_selections) {
        if (!ValidateAnalysisScopeSelectionName(selection.name, error)) return false;
        if (!selectionNames.insert(selection.name).second)
            return fail("Saved selection names must be unique.");
        if (static_cast<std::uint32_t>(selection.sourceKind) >
            static_cast<std::uint32_t>(AnalysisScopeSourceKind::IncludedObservations))
            return fail("A saved selection has an invalid source type.");
        for (int row : selection.originalRowIds)
            if (row <= 0 || row > document.dataset.rows)
                return fail("A saved selection contains an invalid row.");
    }
    std::set<std::string> schemeNames;
    for (const NamedColorScheme &scheme : document.color_schemes) {
        if (scheme.datasetId != document.dataset.group)
            return fail("A saved color scheme belongs to another dataset.");
        if (!ValidateAnalysisScopeSelectionName(scheme.name, error)) return false;
        if (!schemeNames.insert(scheme.name).second)
            return fail("Saved color scheme names must be unique.");
        if (scheme.dataVersion == 0)
            return fail("A saved color scheme has no dataset version.");
        std::set<std::string> ids;
        for (const std::string &id : scheme.scopeStableRowIds)
            if (id.empty() || !ids.insert(id).second)
                return fail("A saved color scheme has invalid case identifiers.");
        for (const auto &[id, color] : scheme.colorsByStableRowId)
            if (!ids.count(id) || color.empty())
                return fail("A saved color scheme contains an invalid case color.");
    }
    return true;
}

bool CreateLinkEDADataDocument(const ApplicationState &state, const std::string &datasetId,
                               LinkEDADataDocument &document, std::string *error)
{
    const DataFrameModel *dataset = state.datasets().find(datasetId);
    if (!dataset) { if (error) *error = "No current dataset is available to save."; return false; }
    LinkEDADataDocument candidate;
    candidate.dataset = *dataset;
    candidate.variable_roles = state.variableRoles(datasetId);
    candidate.row_colors = state.pointColors(datasetId);
    candidate.analysis_scope = state.baseAnalysisScope(datasetId);
    const auto excluded = state.excludedRows(datasetId);
    candidate.excluded_rows.assign(excluded.begin(), excluded.end());
    candidate.analysis_scope.sourceViewId.reset();
    candidate.analysis_scope.sourceElementId.reset();
    candidate.saved_selections = state.savedSelections(datasetId);
    candidate.color_schemes = state.colorSchemes(datasetId);
    for (SavedSelection &selection : candidate.saved_selections)
        selection.sourceViewId.reset();
    candidate.point_label_column = state.labelColumn(datasetId);
    candidate.data_sheet_note = state.documentNote(datasetId);
    if (candidate.data_sheet_note) candidate.data_sheet_note->visible = false;
    candidate.output_code_references = state.outputCodeReferencesForDataset(datasetId);
    candidate.shared_data_versions =
        state.referencedHistoricalDatasetVersions(datasetId);
    if (!ValidateLinkEDADataDocument(candidate, error)) return false;
    document = std::move(candidate); return true;
}

bool ApplyLinkEDADataDocument(const LinkEDADataDocument &document, ApplicationState &state,
                              std::string *error)
{
    if (!ValidateLinkEDADataDocument(document, error)) return false;
    ApplicationState staged;
    if (!staged.registerDataset(document.dataset) ||
        !staged.restoreVariableRoles(document.dataset.group, document.variable_roles, error) ||
        !staged.replaceSavedSelections(document.dataset.group, document.saved_selections, error) ||
        !staged.replaceColorSchemes(document.dataset.group, document.color_schemes, error)) return false;
    AnalysisScope scope = document.analysis_scope; scope.datasetId = document.dataset.group;
    if (!staged.setActiveAnalysisScope(scope, nullptr, error)) return false;
    if (!staged.setExcludedRows(document.dataset.group,
            std::set<int>(document.excluded_rows.begin(), document.excluded_rows.end()),
            nullptr, error)) return false;
    for (const auto &[row, color] : document.row_colors)
        if (!staged.setPointColor(document.dataset.group, row, color)) return false;
    if (!document.point_label_column.empty() &&
        !staged.setLabelColumn(document.dataset.group, document.point_label_column)) return false;
    if (!staged.setDocumentNote(document.dataset.group, document.data_sheet_note)) return false;

    if (state.datasets().contains(document.dataset.group)) state.eraseDataset(document.dataset.group);
    state.registerDataset(document.dataset);
    state.restoreVariableRoles(document.dataset.group, document.variable_roles, nullptr);
    state.replaceSavedSelections(document.dataset.group, document.saved_selections, nullptr);
    state.replaceColorSchemes(document.dataset.group, document.color_schemes, nullptr);
    // Restore the brushing state underlying a live global mode. Older files
    // recorded the scope rows but did not persist a separate selection.
    if (AnalysisScopeTracksCurrentSelection(scope) || scope.sourceKind==AnalysisScopeSourceKind::CurrentUnselection) {
        std::set<int> selection(scope.originalRowIds.begin(),scope.originalRowIds.end());
        if(scope.sourceKind==AnalysisScopeSourceKind::CurrentUnselection) {
            const auto included=selection;selection.clear();
            for(int row=1;row<=document.dataset.rows;++row) if(!included.count(row)) selection.insert(row);
        }
        state.setSelectedRows(document.dataset.group,selection);
    }
    state.setActiveAnalysisScope(scope, nullptr, nullptr);
    state.setExcludedRows(document.dataset.group,
        std::set<int>(document.excluded_rows.begin(), document.excluded_rows.end()));
    for (const auto &[row, color] : document.row_colors)
        state.setPointColor(document.dataset.group, row, color);
    state.setLabelColumn(document.dataset.group, document.point_label_column);
    state.setDocumentNote(document.dataset.group, document.data_sheet_note);
    state.restoreOutputCodeReferences(document.output_code_references);
    state.restoreHistoricalDatasetVersions(document.shared_data_versions);
    return true;
}

std::vector<unsigned char> EncodeLinkEDADataDocument(const LinkEDADataDocument &document,
                                                      std::string *error)
{
    if (!ValidateLinkEDADataDocument(document, error)) return {};
    Writer out; out.string(kPayloadMagic); out.string(document.format); out.u32(document.version);
    const DataFrameModel &data = document.dataset;
    out.string(data.group); out.i64(data.rows); out.string(data.datasetType);
    out.string(data.imputationId); out.string(data.sourceDatasetId); out.i64(data.imputationCount);
    out.i64(data.activeImputationVersion); out.string(data.imputationDisplayMode);
    out.u64(data.columns.size());
    for (const auto &column : data.columns) WriteColumn(out, column, document.version);
    out.u64(document.variable_roles.size());
    for (const auto &[name, role] : document.variable_roles) { out.string(name); out.string(role); }
    out.u64(document.row_colors.size());
    for (const auto &[row, color] : document.row_colors) { out.i64(row); out.string(color); }
    WriteScope(out, document.analysis_scope);
    out.u64(document.saved_selections.size());
    for (const SavedSelection &selection : document.saved_selections) {
        out.string(selection.datasetId); out.string(selection.name); out.ints(selection.originalRowIds);
        out.u32(static_cast<std::uint32_t>(selection.sourceKind)); out.optionalString(selection.sourceViewId);
    }
    out.string(document.point_label_column);
    out.u8(document.data_sheet_note ? 1 : 0);
    if (document.data_sheet_note) {
        const WindowNote &note = *document.data_sheet_note;
        out.string(note.view_id); out.string(note.plain_text); out.u8(note.visible); out.u8(note.has_content);
        out.u8(note.include_in_export); out.i64(TimeValue(note.created_at)); out.i64(TimeValue(note.modified_at));
    }
    if (document.version >= 2) {
        out.strings(data.stableRowIds); out.u64(data.dataVersion);
        WriteDataProvenance(out, data.provenance);
        out.u64(document.output_code_references.size());
        for (const auto &reference : document.output_code_references)
            WriteOutputReference(out, reference, document.version);
    }
    if (document.version >= 3) {
        out.u64(document.shared_data_versions.size());
        for (const DataFrameModel &version : document.shared_data_versions)
            WriteSharedDataFrame(out, version, document.version);
    }
    if (document.version >= 8) out.string(data.imputationProcess);
    if (document.version >= 9) {
        out.u64(document.color_schemes.size());
        for (const NamedColorScheme &scheme : document.color_schemes) {
            out.string(scheme.datasetId); out.string(scheme.name); out.u64(scheme.dataVersion);
            WriteScope(out, scheme.scope); out.strings(scheme.scopeStableRowIds);
            out.u64(scheme.colorsByStableRowId.size());
            for (const auto &[id, color] : scheme.colorsByStableRowId) {
                out.string(id); out.string(color);
            }
        }
    }
    if (document.version >= 10) out.ints(document.excluded_rows);
    return out.take();
}

std::vector<unsigned char> EncodeLinkEDADataChangePayload(
    const LinkEDADataDocument &document, std::string *error)
{
    LinkEDADataDocument dataOnly = document;
    dataOnly.row_colors.clear();
    dataOnly.excluded_rows.clear();
    dataOnly.analysis_scope = AllObservationsAnalysisScope(
        dataOnly.dataset.group, static_cast<std::size_t>(dataOnly.dataset.rows));
    dataOnly.saved_selections.clear();
    dataOnly.color_schemes.clear();
    dataOnly.point_label_column.clear();
    dataOnly.data_sheet_note.reset();
    dataOnly.output_code_references.clear();
    dataOnly.shared_data_versions.clear();
    dataOnly.dataset.activeImputationVersion = 0;
    dataOnly.dataset.imputationDisplayMode.clear();
    return EncodeLinkEDADataDocument(dataOnly, error);
}

bool DecodeLinkEDADataDocument(const std::vector<unsigned char> &payload,
                               LinkEDADataDocument &document, std::string *error)
{
    Reader in(payload); LinkEDADataDocument candidate; std::string magic;
    std::int64_t rows = 0, count = 0, version = 0;
    if (!in.string(magic) || magic != kPayloadMagic || !in.string(candidate.format) ||
        !in.u32(candidate.version) || !in.string(candidate.dataset.group) || !in.i64(rows) ||
        !in.string(candidate.dataset.datasetType) || !in.string(candidate.dataset.imputationId) ||
        !in.string(candidate.dataset.sourceDatasetId) || !in.i64(count)) {
        if (error) *error = in.error().empty() ? "The LinkEDA document payload is malformed." : in.error(); return false;
    }
    candidate.dataset.rows = static_cast<int>(rows); candidate.dataset.imputationCount = static_cast<int>(count);
    if (!in.i64(version) || !in.string(candidate.dataset.imputationDisplayMode)) goto malformed;
    candidate.dataset.activeImputationVersion = static_cast<int>(version);
    {
        std::uint64_t size = 0; if (!in.count(size)) goto malformed;
        candidate.dataset.columns.resize(static_cast<std::size_t>(size));
        for (auto &column : candidate.dataset.columns)
            if (!ReadColumn(in, column, candidate.version)) goto malformed;
        if (!in.count(size)) goto malformed;
        for (std::uint64_t i = 0; i < size; ++i) {
            std::string name, role; if (!in.string(name) || !in.string(role)) goto malformed;
            candidate.variable_roles[std::move(name)] = std::move(role);
        }
        if (!in.count(size)) goto malformed;
        for (std::uint64_t i = 0; i < size; ++i) {
            std::int64_t row = 0; std::string color; if (!in.i64(row) || !in.string(color)) goto malformed;
            candidate.row_colors.emplace_back(static_cast<int>(row), std::move(color));
        }
        if (!ReadScope(in, candidate.analysis_scope) || !in.count(size)) goto malformed;
        candidate.saved_selections.resize(static_cast<std::size_t>(size));
        for (SavedSelection &selection : candidate.saved_selections) {
            std::uint32_t source = 0;
            if (!in.string(selection.datasetId) || !in.string(selection.name) ||
                !in.ints(selection.originalRowIds) || !in.u32(source) ||
                !in.optionalString(selection.sourceViewId)) goto malformed;
            selection.sourceKind = static_cast<AnalysisScopeSourceKind>(source);
        }
    }
    if (!in.string(candidate.point_label_column)) goto malformed;
    {
        std::uint8_t present = 0; if (!in.u8(present) || present > 1) goto malformed;
        if (present) {
            WindowNote note; std::uint8_t visible = 0, content = 0, include = 0;
            std::int64_t created = 0, modified = 0;
            if (!in.string(note.view_id) || !in.string(note.plain_text) || !in.u8(visible) ||
                !in.u8(content) || !in.u8(include) || !in.i64(created) || !in.i64(modified)) goto malformed;
            note.visible = visible != 0; note.has_content = content != 0; note.include_in_export = include != 0;
            note.created_at = TimePoint(created); note.modified_at = TimePoint(modified);
            candidate.data_sheet_note = std::move(note);
        }
    }
    if (candidate.version >= 2) {
        std::uint64_t dataVersion = 0, size = 0;
        if (!in.strings(candidate.dataset.stableRowIds) || !in.u64(dataVersion) ||
            !ReadDataProvenance(in, candidate.dataset.provenance) || !in.count(size)) goto malformed;
        candidate.dataset.dataVersion = dataVersion;
        candidate.output_code_references.resize(static_cast<std::size_t>(size));
        for (auto &reference : candidate.output_code_references)
            if (!ReadOutputReference(in, reference, candidate.version)) goto malformed;
    } else {
        EnsureDataFrameProvenance(candidate.dataset);
    }
    if (candidate.version >= 3) {
        std::uint64_t versionCount = 0;
        if (!in.count(versionCount)) goto malformed;
        candidate.shared_data_versions.resize(static_cast<std::size_t>(versionCount));
        for (DataFrameModel &version : candidate.shared_data_versions)
            if (!ReadSharedDataFrame(in, version, candidate.version)) goto malformed;
    }
    if (candidate.version >= 8 && !in.string(candidate.dataset.imputationProcess)) goto malformed;
    if (candidate.version >= 9) {
        std::uint64_t schemeCount = 0;
        if (!in.count(schemeCount)) goto malformed;
        candidate.color_schemes.resize(static_cast<std::size_t>(schemeCount));
        for (NamedColorScheme &scheme : candidate.color_schemes) {
            std::uint64_t colorCount = 0;
            if (!in.string(scheme.datasetId) || !in.string(scheme.name) ||
                !in.u64(scheme.dataVersion) || !ReadScope(in, scheme.scope) ||
                !in.strings(scheme.scopeStableRowIds) || !in.count(colorCount)) goto malformed;
            for (std::uint64_t i = 0; i < colorCount; ++i) {
                std::string id, color;
                if (!in.string(id) || !in.string(color)) goto malformed;
                scheme.colorsByStableRowId[std::move(id)] = std::move(color);
            }
        }
    }
    if (candidate.version >= 10 && !in.ints(candidate.excluded_rows)) goto malformed;
    if (!in.done()) { if (error) *error = "The LinkEDA document contains unexpected trailing data."; return false; }
    if (!ValidateLinkEDADataDocument(candidate, error)) return false;
    document = std::move(candidate); return true;
malformed:
    if (error) *error = in.error().empty() ? "The LinkEDA document payload is malformed." : in.error();
    return false;
}

std::string NativeLinkEDAWriteRScript()
{
    return R"RLS(args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 2L) stop("Expected payload and output paths.")
con <- file(args[[1L]], "rb"); on.exit(close(con), add=TRUE)
payload <- readBin(con, what="raw", n=file.info(args[[1L]])$size)
document <- list(format="LinkEDA", version=10L,
                 schema="LinkEDADataDocument/v10", payload=payload)
saveRDS(document, args[[2L]], version=3, compress="gzip")
)RLS";
}

std::string NativeLinkEDAReadRScript()
{
    return R"RLS(args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 2L) stop("Expected document and payload paths.")
document <- tryCatch(readRDS(args[[1L]]), error=function(e) stop("Invalid or corrupted .linkeda file: ", conditionMessage(e)))
if (!is.list(document) || !identical(document$format, "LinkEDA")) stop("This is not a LinkEDA data document.")
if (length(document$version) != 1L || is.na(document$version)) stop("The LinkEDA document has no valid format version.")
if (document$version > 10L) stop("This LinkEDA document was created by a newer unsupported version.")
valid_schema <- identical(document$schema, paste0("LinkEDADataDocument/v", as.integer(document$version)))
if (!as.integer(document$version) %in% seq_len(10L) || !valid_schema) stop("Unsupported LinkEDA document version.")
if (!is.raw(document$payload)) stop("The LinkEDA document payload is malformed.")
con <- file(args[[2L]], "wb"); on.exit(close(con), add=TRUE); writeBin(document$payload, con)
)RLS";
}

} // namespace rlispstat::core
