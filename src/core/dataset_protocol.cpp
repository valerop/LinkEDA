#include "dataset_protocol.h"

#include "command_model.h"

#include <algorithm>
#include <cstdlib>
#include <map>
#include <ostream>

namespace rlispstat {
namespace core {

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
        col.type = NormalizeVariableType(lines[cursor++]);
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
            col.values.push_back(lines[cursor++]);
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
                displayValues.push_back(lines[cursor++]);
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
                levels.push_back(lines[cursor++]);
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
                    original[missingRows[static_cast<std::size_t>(ri)]] = lines[cursor++];
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
                        values[missingRows[static_cast<std::size_t>(ri)]] = lines[cursor++];
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
                original.push_back(lines[cursor++]);
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
                    values.push_back(lines[cursor++]);
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

    ApplyImputationDisplayModeToStoredValues(df);
    out = df;
    return true;
}

void WriteDataFramePayloadForR(std::ostream &out,
                               const DataFrameModel &df,
                               bool includeImputation)
{
    out << "DATASET\n" << ProtocolSafeValue(df.group) << "\n"
        << df.rows << "\n" << df.columns.size() << "\n";
    for (const DataColumn &col : df.columns) {
        out << ProtocolSafeValue(col.name) << "\n" << ProtocolSafeValue(col.type) << "\n";
        for (int row = 0; row < df.rows; ++row) {
            std::string value = (row >= 0 && static_cast<std::size_t>(row) < col.values.size())
                ? col.values[static_cast<std::size_t>(row)]
                : "NA";
            out << ProtocolSafeValue(value) << "\n";
        }
    }
    if (!includeImputation || df.datasetType != "multiple_imputation" || df.imputationCount <= 0) {
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
        << ProtocolSafeValue(df.datasetType) << "\n"
        << ProtocolSafeValue(df.imputationId) << "\n"
        << ProtocolSafeValue(df.sourceDatasetId) << "\n"
        << std::max(0, df.imputationCount) << "\n"
        << std::max(1, df.activeImputationVersion) << "\n"
        << ProtocolSafeValue(df.imputationDisplayMode) << "\n"
        << imputedColumns.size() << "\n";
    for (const DataColumn *col : imputedColumns) {
        std::vector<std::size_t> rows;
        for (int row = 0; row < df.rows; ++row) {
            if (DataFrameCellIsImputed(df, *col, static_cast<std::size_t>(row))) {
                rows.push_back(static_cast<std::size_t>(row));
            }
        }
        out << ProtocolSafeValue(col->name) << "\n" << rows.size() << "\n";
        for (std::size_t row : rows) {
            out << (row + 1) << "\n";
        }
        for (std::size_t row : rows) {
            out << ProtocolSafeValue(OriginalImputationValueForCell(*col, row)) << "\n";
        }
        for (int version = 0; version < df.imputationCount; ++version) {
            for (std::size_t row : rows) {
                out << ProtocolSafeValue(ImputationVersionValueForCell(*col, row, static_cast<std::size_t>(version))) << "\n";
            }
        }
    }
}

} // namespace core
} // namespace rlispstat
