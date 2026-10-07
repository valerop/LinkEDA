#include "dataset_protocol.h"

#include "command_model.h"

#include <algorithm>
#include <cctype>
#include <cstdlib>
#include <map>
#include <ostream>

namespace rlispstat {
namespace core {

namespace {

bool DecodeProvenanceHex(const std::string &encoded, std::string &decoded)
{
    if (encoded.size() % 2U != 0U) return false;
    decoded.clear();
    decoded.reserve(encoded.size() / 2U);
    auto nibble = [](unsigned char value) -> int {
        if (value >= '0' && value <= '9') return value - '0';
        if (value >= 'a' && value <= 'f') return value - 'a' + 10;
        if (value >= 'A' && value <= 'F') return value - 'A' + 10;
        return -1;
    };
    for (std::size_t index = 0; index < encoded.size(); index += 2U) {
        const int high = nibble(static_cast<unsigned char>(encoded[index]));
        const int low = nibble(static_cast<unsigned char>(encoded[index + 1U]));
        if (high < 0 || low < 0) return false;
        decoded.push_back(static_cast<char>((high << 4) | low));
    }
    return true;
}

bool ReadCount(const std::vector<std::string> &lines, std::size_t &cursor,
               std::size_t &value)
{
    if (cursor >= lines.size()) return false;
    char *end = nullptr;
    const unsigned long long parsed = std::strtoull(lines[cursor].c_str(), &end, 10);
    if (!end || end == lines[cursor].c_str() || *end != '\0') return false;
    ++cursor;
    value = static_cast<std::size_t>(parsed);
    return true;
}

bool ReadStrings(const std::vector<std::string> &lines, std::size_t &cursor,
                 std::vector<std::string> &values)
{
    std::size_t count = 0;
    if (!ReadCount(lines, cursor, count) || cursor + count > lines.size()) return false;
    values.assign(lines.begin() + static_cast<std::ptrdiff_t>(cursor),
                  lines.begin() + static_cast<std::ptrdiff_t>(cursor + count));
    cursor += count;
    return true;
}

bool UsesOneBasedFactorCodes(const DataColumn &column)
{
    if (!VariableTypeIsFactorLike(column.type) || column.definedLevels.empty()) return false;
    bool observed = false;
    for (const std::string &value : column.values) {
        if (DataCellIsMissing(value)) continue;
        char *end = nullptr;
        const long code = std::strtol(value.c_str(), &end, 10);
        if (!end || end == value.c_str() || *end != '\0' || code < 1 ||
            static_cast<std::size_t>(code) > column.definedLevels.size()) return false;
        observed = true;
    }
    return observed;
}

std::string FactorLabelForR(const DataColumn &column, const std::string &value,
                            bool oneBasedCodes)
{
    if (!oneBasedCodes || DataCellIsMissing(value)) return value;
    char *end = nullptr;
    const long code = std::strtol(value.c_str(), &end, 10);
    if (!end || end == value.c_str() || *end != '\0' || code < 1 ||
        static_cast<std::size_t>(code) > column.definedLevels.size()) return value;
    return column.definedLevels[static_cast<std::size_t>(code - 1)];
}

std::string AnalysisValueForR(const DataColumn &column, std::size_t row,
                              bool oneBasedCodes)
{
    if (!VariableTypeIsFactorLike(column.type))
        return row < column.values.size() ? column.values[row] : "NA";
    if (row < column.displayValues.size() &&
        !DataCellIsMissing(column.displayValues[row])) return column.displayValues[row];
    const std::string value = row < column.values.size() ? column.values[row] : "NA";
    return FactorLabelForR(column, value, oneBasedCodes);
}

std::vector<std::string> AnalysisFactorLevelsForR(const DataColumn &column,
                                                  bool oneBasedCodes)
{
    std::vector<std::string> levels;
    auto append = [&](const std::string &value) {
        if (!DataCellIsMissing(value) &&
            std::find(levels.begin(), levels.end(), value) == levels.end())
            levels.push_back(value);
    };
    for (const std::string &level : column.definedLevels) append(level);
    for (std::size_t row = 0; row < column.values.size(); ++row)
        append(AnalysisValueForR(column, row, oneBasedCodes));
    return levels;
}

} // namespace

bool ParseDataFramePayload(const std::vector<std::string> &lines,
                           std::size_t &cursor,
                           const std::string &group,
                           DataFrameModel &out,
                           bool *parsed,
                           std::string &error)
{
    if (parsed) {
        *parsed = false;
    }
    if (cursor >= lines.size() || lines[cursor] != "DATAFRAME") {
        return true;
    }
    if (parsed) {
        *parsed = true;
    }

    ++cursor;
    if (cursor + 1 >= lines.size()) {
        error = "ERR malformed dataframe payload";
        return false;
    }
    long rowCount = std::strtol(lines[cursor++].c_str(), nullptr, 10);
    long colCount = std::strtol(lines[cursor++].c_str(), nullptr, 10);
    if (rowCount < 0 || colCount < 0) {
        error = "ERR invalid dataframe dimensions";
        return false;
    }

    const bool encodedCells = cursor < lines.size() && lines[cursor] == "DATACELLS_PERCENT_V1";
    if (encodedCells) ++cursor;
    const auto readCell = [&]() {
        const std::string value = lines[cursor++];
        return encodedCells ? DecodeCommandField(value) : value;
    };
    DataFrameModel df;
    df.group = group;
    df.rows = static_cast<int>(rowCount);
    for (long ci = 0; ci < colCount; ++ci) {
        if (cursor + 1 >= lines.size()) {
            error = "ERR malformed dataframe column";
            return false;
        }
        DataColumn col;
        col.name = lines[cursor++];
        const std::string transportedType = NormalizeVariableType(lines[cursor++]);
        // `logical` is retained only as an R storage representation.  Its
        // statistical meaning in LinkEDA is binary Categorical.
        col.type = transportedType == "logical" ? "factor" : transportedType;
        col.storageType = transportedType == "logical" ? "logical" :
            transportedType == "numeric" ? "double" :
            transportedType == "ordered" ? "ordered factor" :
            transportedType == "factor" ? "factor" : "character";
        if (!VariableTypeIsSupported(col.type)) {
            col.type = "character";
        }
        col.displayName = col.name;
        if (cursor + static_cast<std::size_t>(rowCount) > lines.size()) {
            error = "ERR malformed dataframe values";
            return false;
        }
        col.values.reserve(static_cast<std::size_t>(rowCount));
        for (long ri = 0; ri < rowCount; ++ri) {
            col.values.push_back(readCell());
        }
        df.columns.push_back(col);
    }

    if (cursor < lines.size() && lines[cursor] == "DATADISPLAY") {
        ++cursor;
        if (cursor >= lines.size()) {
            error = "ERR malformed dataframe display payload";
            return false;
        }
        long displayCount = std::strtol(lines[cursor++].c_str(), nullptr, 10);
        if (displayCount < 0) {
            error = "ERR invalid dataframe display count";
            return false;
        }
        for (long di = 0; di < displayCount; ++di) {
            if (cursor >= lines.size()) {
                error = "ERR malformed dataframe display column";
                return false;
            }
            std::string name = lines[cursor++];
            if (cursor + static_cast<std::size_t>(rowCount) > lines.size()) {
                error = "ERR malformed dataframe display values";
                return false;
            }
            std::vector<std::string> displayValues;
            displayValues.reserve(static_cast<std::size_t>(rowCount));
            for (long ri = 0; ri < rowCount; ++ri) {
                displayValues.push_back(readCell());
            }
            for (DataColumn &col : df.columns) {
                if (col.name == name) {
                    col.displayValues = displayValues;
                    break;
                }
            }
        }
    }

    if (cursor < lines.size() && lines[cursor] == "DATLEVELS") {
        ++cursor;
        if (cursor >= lines.size()) {
            error = "ERR malformed dataframe levels payload";
            return false;
        }
        long levelColumnCount = std::strtol(lines[cursor++].c_str(), nullptr, 10);
        if (levelColumnCount < 0) {
            error = "ERR invalid dataframe levels count";
            return false;
        }
        for (long li = 0; li < levelColumnCount; ++li) {
            if (cursor + 1 >= lines.size()) {
                error = "ERR malformed dataframe levels column";
                return false;
            }
            std::string name = lines[cursor++];
            long levelCount = std::strtol(lines[cursor++].c_str(), nullptr, 10);
            if (levelCount < 0 || cursor + static_cast<std::size_t>(levelCount) > lines.size()) {
                error = "ERR malformed dataframe levels values";
                return false;
            }
            std::vector<std::string> levels;
            levels.reserve(static_cast<std::size_t>(levelCount));
            for (long levelIndex = 0; levelIndex < levelCount; ++levelIndex) {
                levels.push_back(readCell());
            }
            for (DataColumn &col : df.columns) {
                if (col.name == name) {
                    col.definedLevels = std::move(levels);
                    break;
                }
            }
        }
    }

    if (cursor < lines.size() && lines[cursor] == "DATAMETA") {
        ++cursor;
        if (cursor >= lines.size()) {
            error = "ERR malformed dataframe metadata payload";
            return false;
        }
        long metaCount = std::strtol(lines[cursor++].c_str(), nullptr, 10);
        if (metaCount < 0) {
            error = "ERR invalid dataframe metadata count";
            return false;
        }
        for (long mi = 0; mi < metaCount; ++mi) {
            if (cursor + 3 >= lines.size()) {
                error = "ERR malformed dataframe metadata row";
                return false;
            }
            std::string name = lines[cursor++];
            std::string displayName = DecodeCommandField(lines[cursor++]);
            std::string description = DecodeCommandField(lines[cursor++]);
            int decimals = std::atoi(lines[cursor++].c_str());
            for (DataColumn &col : df.columns) {
                if (col.name == name) {
                    col.displayName = displayName.empty() ? col.name : displayName;
                    col.description = description == "NA" ? "" : description;
                    col.decimals = decimals;
                    break;
                }
            }
        }
    }

    if (cursor < lines.size() && lines[cursor] == "DATATYPEMETA") {
        ++cursor;
        std::size_t typeMetaCount = 0;
        if (!ReadCount(lines, cursor, typeMetaCount)) {
            error = "ERR malformed dataframe type metadata payload";
            return false;
        }
        for (std::size_t mi = 0; mi < typeMetaCount; ++mi) {
            if (cursor + 4 >= lines.size()) {
                error = "ERR malformed dataframe type metadata row";
                return false;
            }
            const std::string name = lines[cursor++];
            std::string semantic = NormalizeVariableType(lines[cursor++]);
            if (semantic == "logical") semantic = "factor";
            const std::string storage = DecodeCommandField(lines[cursor++]);
            const std::string binaryText = lines[cursor++];
            const std::string reversibleType = NormalizeVariableType(lines[cursor++]);
            std::vector<std::string> categoryOrder;
            if (!ReadStrings(lines, cursor, categoryOrder)) {
                error = "ERR malformed dataframe category order";
                return false;
            }
            for (std::string &level : categoryOrder) level = DecodeCommandField(level);
            std::size_t mappingCount = 0;
            if (!ReadCount(lines, cursor, mappingCount) ||
                cursor + mappingCount * 2U > lines.size()) {
                error = "ERR malformed dataframe numeric mapping";
                return false;
            }
            std::map<std::string, std::string> mapping;
            for (std::size_t mappingIndex = 0; mappingIndex < mappingCount; ++mappingIndex) {
                std::string label = DecodeCommandField(lines[cursor++]);
                std::string value = DecodeCommandField(lines[cursor++]);
                mapping[std::move(label)] = std::move(value);
            }
            DataColumn *column = FindDataColumnInDataFrame(df, name);
            if (!column) continue;
            if (VariableTypeIsSupported(semantic)) column->type = semantic;
            column->storageType = storage;
            column->binary = binaryText == "1";
            column->numericMapping = std::move(mapping);
            column->reversibleCategoryType =
                (reversibleType == "factor" || reversibleType == "ordered")
                    ? reversibleType : std::string{};
            if (VariableTypeIsFactorLike(column->type)) {
                column->definedLevels = std::move(categoryOrder);
            } else if (column->type == "numeric" && !column->numericMapping.empty()) {
                column->reversibleFactorLevels = std::move(categoryOrder);
            }
        }
    }

    if (cursor < lines.size() && (lines[cursor] == "IMPUTATION" || lines[cursor] == "IMPUTATION_SPARSE")) {
        bool sparseImputationPayload = lines[cursor] == "IMPUTATION_SPARSE";
        ++cursor;
        if (cursor + 6 >= lines.size()) {
            error = "ERR malformed imputation payload";
            return false;
        }
        df.datasetType = lines[cursor++];
        df.imputationId = lines[cursor++];
        df.sourceDatasetId = lines[cursor++];
        df.imputationCount = std::atoi(lines[cursor++].c_str());
        df.activeImputationVersion = std::atoi(lines[cursor++].c_str());
        df.imputationDisplayMode = lines[cursor++];
        long miColCount = std::strtol(lines[cursor++].c_str(), nullptr, 10);
        if (miColCount < 0 || df.imputationCount < 0) {
            error = "ERR invalid imputation metadata";
            return false;
        }
        for (long ci = 0; ci < miColCount; ++ci) {
            if (cursor >= lines.size()) {
                error = "ERR malformed imputation column";
                return false;
            }
            std::string name = lines[cursor++];
            DataColumn *target = FindDataColumnInDataFrame(df, name);
            if (sparseImputationPayload) {
                if (cursor >= lines.size()) {
                    error = "ERR malformed sparse imputation row count";
                    return false;
                }
                long missingCount = std::strtol(lines[cursor++].c_str(), nullptr, 10);
                if (missingCount < 0 || cursor + static_cast<std::size_t>(missingCount) > lines.size()) {
                    error = "ERR invalid sparse imputation row count";
                    return false;
                }
                std::vector<std::size_t> missingRows;
                missingRows.reserve(static_cast<std::size_t>(missingCount));
                std::vector<bool> missing(static_cast<std::size_t>(rowCount), false);
                for (long ri = 0; ri < missingCount; ++ri) {
                    long rowNumber = std::strtol(lines[cursor++].c_str(), nullptr, 10);
                    if (rowNumber < 1 || rowNumber > rowCount) {
                        error = "ERR sparse imputation row out of range";
                        return false;
                    }
                    std::size_t rowIndex = static_cast<std::size_t>(rowNumber - 1);
                    missingRows.push_back(rowIndex);
                    missing[rowIndex] = true;
                }
                if (cursor + static_cast<std::size_t>(missingCount) > lines.size()) {
                    error = "ERR malformed sparse imputation originals";
                    return false;
                }
                std::map<std::size_t, std::string> original;
                for (long ri = 0; ri < missingCount; ++ri) {
                    original[missingRows[static_cast<std::size_t>(ri)]] = readCell();
                }
                std::vector<std::map<std::size_t, std::string>> versions;
                versions.reserve(static_cast<std::size_t>(df.imputationCount));
                for (int version = 0; version < df.imputationCount; ++version) {
                    if (cursor + static_cast<std::size_t>(missingCount) > lines.size()) {
                        error = "ERR malformed sparse imputation version values";
                        return false;
                    }
                    std::map<std::size_t, std::string> values;
                    for (long ri = 0; ri < missingCount; ++ri) {
                        values[missingRows[static_cast<std::size_t>(ri)]] = readCell();
                    }
                    versions.push_back(values);
                }
                if (target) {
                    target->imputedMissing = missing;
                    target->imputationOriginalSparse = original;
                    target->imputationValuesSparse = versions;
                }
                continue;
            }

            if (cursor + static_cast<std::size_t>(rowCount) * 2 > lines.size()) {
                error = "ERR malformed imputation cell metadata";
                return false;
            }
            std::vector<bool> missing;
            missing.reserve(static_cast<std::size_t>(rowCount));
            for (long ri = 0; ri < rowCount; ++ri) {
                missing.push_back(lines[cursor++] == "1");
            }
            std::vector<std::string> original;
            original.reserve(static_cast<std::size_t>(rowCount));
            for (long ri = 0; ri < rowCount; ++ri) {
                original.push_back(readCell());
            }
            std::vector<std::vector<std::string>> versions;
            versions.reserve(static_cast<std::size_t>(df.imputationCount));
            for (int version = 0; version < df.imputationCount; ++version) {
                if (cursor + static_cast<std::size_t>(rowCount) > lines.size()) {
                    error = "ERR malformed imputation version values";
                    return false;
                }
                std::vector<std::string> values;
                values.reserve(static_cast<std::size_t>(rowCount));
                for (long ri = 0; ri < rowCount; ++ri) {
                    values.push_back(readCell());
                }
                versions.push_back(values);
            }
            if (target) {
                target->imputedMissing = missing;
                target->imputationOriginalValues = original;
                target->imputationValues = versions;
            }
        }
    }

    if (cursor < lines.size() && lines[cursor] == "IMPUTATION_PROCESS_V1") {
        if (++cursor >= lines.size()) {
            error = "ERR incomplete imputation process metadata";
            return false;
        }
        df.imputationProcess = lines[cursor++];
    }
    if (cursor < lines.size() && lines[cursor] == "DATAPROVENANCE_V1") {
        ++cursor;
        std::size_t version = 0;
        if (!ReadCount(lines, cursor, version) || version < 1U || cursor + 3U > lines.size()) {
            error = "ERR malformed data provenance header";
            return false;
        }
        df.dataVersion = static_cast<std::uint64_t>(version);
        df.provenance.origin = RCodeOriginFromId(lines[cursor++]);
        if (!DecodeProvenanceHex(lines[cursor++], df.provenance.originCode)) {
            error = "ERR malformed data provenance R code";
            return false;
        }
        df.provenance.originDescription = lines[cursor++];
        if (!ReadStrings(lines, cursor, df.stableRowIds) ||
            df.stableRowIds.size() != static_cast<std::size_t>(rowCount)) {
            error = "ERR malformed stable row identities";
            return false;
        }
        std::size_t historyCount = 0;
        if (!ReadCount(lines, cursor, historyCount)) {
            error = "ERR malformed data provenance history count";
            return false;
        }
        df.provenance.history.clear();
        df.provenance.columnSteps.clear();
        for (std::size_t index = 0; index < historyCount; ++index) {
            if (cursor + 4U > lines.size()) {
                error = "ERR malformed data provenance transformation";
                return false;
            }
            TransformationStep step;
            step.id = lines[cursor++];
            step.label = lines[cursor++];
            step.origin = RCodeOriginFromId(lines[cursor++]);
            if (!DecodeProvenanceHex(lines[cursor++], step.rCode) ||
                !ReadStrings(lines, cursor, step.parentVersionKeys) ||
                !ReadStrings(lines, cursor, step.inputColumns) ||
                !ReadStrings(lines, cursor, step.outputColumns) ||
                !ReadStrings(lines, cursor, step.stableRowIds)) {
                error = "ERR malformed data provenance transformation payload";
                return false;
            }
            for (const std::string &column : step.outputColumns)
                df.provenance.columnSteps[column].push_back(step.id);
            df.provenance.history.push_back(std::move(step));
        }
        df.provenance.currentVersion = DefaultDataVersionReference(
            group, df.datasetType, df.imputationCount, df.dataVersion);
    }
    if (cursor < lines.size() && lines[cursor] == "DATA_SYNC_SESSION_V1") {
        ++cursor;
        if (cursor >= lines.size()) {
            error = "ERR malformed data sync session";
            return false;
        }
        df.syncSessionToken = lines[cursor++];
    }

    ApplyImputationDisplayModeToStoredValues(df);
    out = df;
    return true;
}

void WriteDataFramePayloadForR(std::ostream &out,
                               const DataFrameModel &df,
                               bool includeImputation)
{
    out << "DATASET\n" << EncodeCommandField(df.group) << "\n"
        << df.rows << "\n" << df.columns.size() << "\nDATACELLS_PERCENT_V1\n";
    for (const DataColumn &col : df.columns) {
        out << EncodeCommandField(col.name) << "\n" << EncodeCommandField(col.type) << "\n";
        const bool oneBasedCodes = UsesOneBasedFactorCodes(col);
        for (int row = 0; row < df.rows; ++row) {
            const std::string value = row >= 0
                ? AnalysisValueForR(col, static_cast<std::size_t>(row), oneBasedCodes)
                : "NA";
            out << EncodeCommandField(value) << "\n";
        }
    }
    std::vector<std::pair<std::string, std::vector<std::string>>> levelColumns;
    for (const DataColumn &col : df.columns) {
        if (!VariableTypeIsFactorLike(col.type)) continue;
        std::vector<std::string> levels = AnalysisFactorLevelsForR(
            col, UsesOneBasedFactorCodes(col));
        if (!levels.empty()) levelColumns.push_back({col.name, std::move(levels)});
    }
    if (!levelColumns.empty()) {
        out << "DATLEVELS\n" << levelColumns.size() << "\n";
        for (const auto &[name, levels] : levelColumns) {
            out << EncodeCommandField(name) << "\n" << levels.size() << "\n";
            for (const std::string &level : levels)
                out << EncodeCommandField(level) << "\n";
        }
    }
    if (!includeImputation || df.datasetType != "multiple_imputation" || df.imputationCount <= 0) {
        out << "DATA_VERSION_V1\n" << df.dataVersion << "\n";
        return;
    }

    std::vector<const DataColumn *> imputedColumns;
    for (const DataColumn &col : df.columns) {
        bool anyImputed = false;
        for (int row = 0; row < df.rows; ++row) {
            if (DataFrameCellIsImputed(df, col, static_cast<std::size_t>(row))) {
                anyImputed = true;
                break;
            }
        }
        if (anyImputed) {
            imputedColumns.push_back(&col);
        }
    }

    out << "IMPUTATION_SPARSE\n"
        << EncodeCommandField(df.datasetType) << "\n"
        << EncodeCommandField(df.imputationId) << "\n"
        << EncodeCommandField(df.sourceDatasetId) << "\n"
        << std::max(0, df.imputationCount) << "\n"
        << std::max(1, df.activeImputationVersion) << "\n"
        << EncodeCommandField(df.imputationDisplayMode) << "\n"
        << imputedColumns.size() << "\n";
    for (const DataColumn *col : imputedColumns) {
        const bool oneBasedCodes = UsesOneBasedFactorCodes(*col);
        std::vector<std::size_t> rows;
        for (int row = 0; row < df.rows; ++row) {
            if (DataFrameCellIsImputed(df, *col, static_cast<std::size_t>(row))) {
                rows.push_back(static_cast<std::size_t>(row));
            }
        }
        out << EncodeCommandField(col->name) << "\n" << rows.size() << "\n";
        for (std::size_t row : rows) {
            out << (row + 1) << "\n";
        }
        for (std::size_t row : rows) {
            out << EncodeCommandField(FactorLabelForR(
                *col, OriginalImputationValueForCell(*col, row), oneBasedCodes)) << "\n";
        }
        for (int version = 0; version < df.imputationCount; ++version) {
            for (std::size_t row : rows) {
                out << EncodeCommandField(FactorLabelForR(
                    *col,
                    ImputationVersionValueForCell(
                        *col, row, static_cast<std::size_t>(version)),
                    oneBasedCodes)) << "\n";
            }
        }
    }
    if (!df.imputationProcess.empty())
        out << "IMPUTATION_PROCESS_V1\n" << df.imputationProcess << "\n";
    out << "DATA_VERSION_V1\n" << df.dataVersion << "\n";
}

} // namespace core
} // namespace rlispstat
