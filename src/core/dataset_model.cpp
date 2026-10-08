#include "dataset_model.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdlib>
#include <iomanip>
#include <ostream>
#include <set>
#include <sstream>

namespace rlispstat {
namespace core {

namespace {

void RefreshCurrentVersion(DataFrameModel &df)
{
    df.dataVersion = std::max<std::uint64_t>(1, df.dataVersion);
    df.provenance.currentVersion = DefaultDataVersionReference(
        df.group, df.datasetType, df.imputationCount, df.dataVersion);
}

std::string NextTransformationId(const DataFrameModel &df)
{
    return df.group + ":transformation:" +
        std::to_string(df.provenance.history.size() + 1);
}

std::string TrimCopy(const std::string &text)
{
    std::size_t start = 0;
    while (start < text.size() && std::isspace(static_cast<unsigned char>(text[start]))) {
        ++start;
    }
    std::size_t end = text.size();
    while (end > start && std::isspace(static_cast<unsigned char>(text[end - 1]))) {
        --end;
    }
    return text.substr(start, end - start);
}

std::string LowerCopy(const std::string &text)
{
    std::string out = text;
    std::transform(out.begin(), out.end(), out.begin(),
                   [](unsigned char ch) { return static_cast<char>(std::tolower(ch)); });
    return out;
}

std::string JoinValues(const std::vector<std::string> &values, const std::string &separator)
{
    std::ostringstream out;
    for (std::size_t i = 0; i < values.size(); ++i) {
        if (i) {
            out << separator;
        }
        out << values[i];
    }
    return out.str();
}

std::string LimitInlineCellText(const std::string &text, std::size_t maxChars)
{
    if (text.size() <= maxChars) {
        return text;
    }
    if (maxChars <= 3) {
        return text.substr(0, maxChars);
    }
    return text.substr(0, maxChars - 3) + "...";
}

std::string UnformattedDisplayValue(const DataColumn &column, std::size_t row)
{
    if (row < column.displayValues.size()) return column.displayValues[row];
    return row < column.values.size() ? column.values[row] : std::string{};
}

std::vector<std::string> ObservedDisplayLevels(const DataColumn &column)
{
    std::vector<std::string> levels;
    auto append = [&](const std::string &value) {
        if (DataCellIsMissing(value) ||
            std::find(levels.begin(), levels.end(), value) != levels.end()) return;
        levels.push_back(value);
    };
    for (const std::string &level : column.definedLevels) append(level);
    for (std::size_t row = 0; row < column.values.size(); ++row)
        append(UnformattedDisplayValue(column, row));
    return levels;
}

bool BinaryNumericCode(const std::string &value, std::size_t &index)
{
    if (DataCellIsMissing(value)) return true;
    double parsed = NAN;
    if (!ParseDataCellDouble(value, parsed)) return false;
    if (std::fabs(parsed) <= 1.0e-9) {
        index = 0;
        return true;
    }
    if (std::fabs(parsed - 1.0) <= 1.0e-9) {
        index = 1;
        return true;
    }
    return false;
}

bool NumericValuesEquivalent(const std::string &left, const std::string &right)
{
    double a = NAN, b = NAN;
    return ParseDataCellDouble(left, a) && ParseDataCellDouble(right, b) &&
        std::fabs(a - b) <= 1.0e-9;
}

bool HasReversibleCategoryEncoding(const DataColumn &column)
{
    if (NormalizeVariableType(column.type) != "numeric" ||
        column.reversibleFactorLevels.empty() || column.numericMapping.empty() ||
        !column.displayValues.empty()) return false;
    auto valid = [](const std::string &value) {
        return DataCellIsMissing(value) || ParseOptionalDataCellDouble(value) ==
            ParseOptionalDataCellDouble(value);
    };
    if (!std::all_of(column.values.begin(), column.values.end(), valid) ||
        !std::all_of(column.imputationOriginalValues.begin(),
                     column.imputationOriginalValues.end(), valid)) return false;
    for (const auto &version : column.imputationValues)
        if (!std::all_of(version.begin(), version.end(), valid)) return false;
    for (const auto &[row, value] : column.imputationOriginalSparse) {
        (void)row;
        if (!valid(value)) return false;
    }
    for (const auto &version : column.imputationValuesSparse)
        for (const auto &[row, value] : version) {
            (void)row;
            if (!valid(value)) return false;
        }
    for (const std::string &level : column.reversibleFactorLevels)
        if (column.numericMapping.find(level) == column.numericMapping.end()) return false;
    return true;
}

std::string RestoreCategoryLabel(const std::string &value,
                                 const DataColumn &column)
{
    if (DataCellIsMissing(value)) return "NA";
    for (const std::string &level : column.reversibleFactorLevels) {
        const auto mapped = column.numericMapping.find(level);
        if (mapped != column.numericMapping.end() &&
            NumericValuesEquivalent(value, mapped->second)) return level;
    }
    return value;
}

bool EncodeCategoricalColumn(DataColumn &column, std::string *message)
{
    const bool restoreLabels = HasReversibleCategoryEncoding(column);
    std::vector<std::string> levels = restoreLabels
        ? column.reversibleFactorLevels : ObservedDisplayLevels(column);
    const bool alreadyEncodedFactor = VariableTypeIsFactorLike(column.type) &&
        !column.displayValues.empty();
    auto appendImputationLevel = [&](const std::string &value) {
        if (alreadyEncodedFactor || restoreLabels || DataCellIsMissing(value) ||
            std::find(levels.begin(), levels.end(), value) != levels.end()) return;
        levels.push_back(value);
    };
    // The active completed dataset may not contain every value occurring in
    // the other imputations.  When a numeric MI variable becomes categorical,
    // establish one canonical level set for the complete MI object rather than
    // letting R discover different levels later during pooling.
    for (const std::string &value : column.imputationOriginalValues)
        appendImputationLevel(value);
    for (const auto &version : column.imputationValues)
        for (const std::string &value : version) appendImputationLevel(value);
    for (const auto &[row, value] : column.imputationOriginalSparse) {
        (void)row;
        appendImputationLevel(value);
    }
    for (const auto &version : column.imputationValuesSparse)
        for (const auto &[row, value] : version) {
            (void)row;
            appendImputationLevel(value);
        }
    std::map<std::string, std::size_t> codes;
    for (std::size_t index = 0; index < levels.size(); ++index)
        codes[levels[index]] = index + 1;

    std::vector<std::string> values;
    std::vector<std::string> display;
    values.reserve(column.values.size());
    display.reserve(column.values.size());
    for (std::size_t row = 0; row < column.values.size(); ++row) {
        const std::string label = restoreLabels
            ? RestoreCategoryLabel(column.values[row], column)
            : UnformattedDisplayValue(column, row);
        if (DataCellIsMissing(label)) {
            values.push_back("NA");
            display.push_back("NA");
            continue;
        }
        const auto found = codes.find(label);
        if (found == codes.end()) {
            if (message) *message = "Could not assign a categorical code to `" + label + "`.";
            return false;
        }
        values.push_back(std::to_string(found->second));
        display.push_back(label);
    }
    column.values = std::move(values);
    column.displayValues = std::move(display);
    column.definedLevels = levels;
    if (restoreLabels) {
        auto restore = [&](std::string &value) {
            value = RestoreCategoryLabel(value, column);
        };
        for (std::string &value : column.imputationOriginalValues) restore(value);
        for (auto &version : column.imputationValues)
            for (std::string &value : version) restore(value);
        for (auto &[row, value] : column.imputationOriginalSparse) {
            (void)row;
            restore(value);
        }
        for (auto &version : column.imputationValuesSparse)
            for (auto &[row, value] : version) {
                (void)row;
                restore(value);
            }
    }
    column.reversibleFactorLevels.clear();
    column.numericMapping.clear();
    column.reversibleCategoryType.clear();
    return true;
}

bool ConvertColumnToNumeric(DataColumn &column, std::string *message,
                            const VariableTypeConversionSpecification *specification)
{
    if (NormalizeVariableType(column.type) == "numeric") return true;
    const std::string sourceType = NormalizeVariableType(column.type);

    std::vector<std::string> displayed;
    displayed.reserve(column.values.size());
    std::vector<double> parsedValues;
    parsedValues.reserve(column.values.size());
    std::vector<bool> parsedRows;
    parsedRows.reserve(column.values.size());
    bool allNumeric = true;
    std::size_t observedCount = 0;
    std::size_t numericCount = 0;
    for (std::size_t row = 0; row < column.values.size(); ++row) {
        const std::string value = UnformattedDisplayValue(column, row);
        displayed.push_back(value);
        double parsed = NAN;
        const bool missing = DataCellIsMissing(value);
        const bool parsedOk = !missing && ParseDataCellDouble(value, parsed);
        parsedRows.push_back(parsedOk);
        parsedValues.push_back(parsed);
        if (!missing) {
            ++observedCount;
            if (parsedOk) ++numericCount;
            else allNumeric = false;
        }
    }

    std::vector<std::string> sourceLevels = ObservedDisplayLevels(column);
    if (specification && !specification->categoryOrder.empty()) {
        std::set<std::string> provided(specification->categoryOrder.begin(),
                                      specification->categoryOrder.end());
        std::set<std::string> existing(sourceLevels.begin(), sourceLevels.end());
        if (provided != existing || provided.size() != specification->categoryOrder.size()) {
            if (message) *message = "Category order must contain every category exactly once.";
            return false;
        }
        sourceLevels = specification->categoryOrder;
    }
    auto semanticValue = [&](const std::string &stored) {
        if (DataCellIsMissing(stored)) return std::string("NA");
        if (std::find(sourceLevels.begin(), sourceLevels.end(), stored) != sourceLevels.end())
            return stored;
        // Factor columns use one-based native codes in `values`, while MI
        // payloads use their labels.  Accept either representation so a type
        // change applies to every completed dataset, including documents
        // written by earlier LinkEDA versions.
        double code = NAN;
        if (!sourceLevels.empty() && ParseDataCellDouble(stored, code) &&
            std::fabs(code - std::round(code)) <= 1.0e-9 && code >= 1.0 &&
            code <= static_cast<double>(sourceLevels.size())) {
            return sourceLevels[static_cast<std::size_t>(std::llround(code)) - 1U];
        }
        return stored;
    };

    std::vector<std::string> converted;
    converted.reserve(displayed.size());
    // Explicitly changing Text to Numeric is also a request to clean a
    // mostly-numeric imported text column.  A handful of survey-entry errors
    // must not make an otherwise numeric variable impossible to use, while a
    // genuinely categorical column must never be silently recoded this way.
    const bool mostlyNumeric = sourceType == "character" && observedCount >= 4 && numericCount >= 4 &&
        static_cast<double>(numericCount) / static_cast<double>(observedCount) >= 0.90;
    std::map<std::string, std::string> mapping;
    if (!allNumeric && !mostlyNumeric && specification)
        mapping = specification->numericMapping;
    if (!allNumeric && !mostlyNumeric && mapping.empty() && sourceType == "ordered" &&
        (!specification || specification->useOrdinalPositions)) {
        for (std::size_t index = 0; index < sourceLevels.size(); ++index)
            mapping[sourceLevels[index]] = std::to_string(index + 1);
        if (message) *message = "This conversion treats consecutive ordinal categories as equally spaced.";
    }
    if (!allNumeric && !mostlyNumeric && mapping.empty() && sourceLevels.size() == 2) {
        mapping[sourceLevels[0]] = specification && specification->invertBinary ? "1" : "0";
        mapping[sourceLevels[1]] = specification && specification->invertBinary ? "0" : "1";
        if (message) *message = "Numeric mapping: " + sourceLevels[0] + " = " +
            mapping[sourceLevels[0]] + "; " + sourceLevels[1] + " = " +
            mapping[sourceLevels[1]] + ".";
    }
    if (!mapping.empty()) {
        if (mapping.size() != sourceLevels.size()) {
            if (message) *message = "The numeric mapping must contain every category exactly once.";
            return false;
        }
        for (const std::string &level : sourceLevels) {
            auto found = mapping.find(level);
            double parsed = NAN;
            if (found == mapping.end() || !ParseDataCellDouble(found->second, parsed)) {
                if (message) *message = "The numeric mapping must assign a valid number to category `" + level + "`.";
                return false;
            }
        }
    }
    if (allNumeric || mostlyNumeric) {
        std::size_t introducedMissing = 0;
        for (std::size_t row = 0; row < displayed.size(); ++row) {
            const std::string &value = displayed[row];
            if (DataCellIsMissing(value)) {
                converted.push_back("NA");
                continue;
            }
            if (!parsedRows[row]) {
                converted.push_back("NA");
                ++introducedMissing;
                continue;
            }
            std::ostringstream normalized;
            normalized << std::setprecision(17) << parsedValues[row];
            converted.push_back(normalized.str());
        }
        if (introducedMissing > 0 && message) {
            *message = column.name + " is now treated as Numeric; " +
                std::to_string(introducedMissing) + " non-numeric " +
                (introducedMissing == 1 ? "value was" : "values were") +
                " set to missing.";
        }
    } else {
        const std::vector<std::string> &levels = sourceLevels;
        if (mapping.empty()) {
            if (message) {
                *message = "Variable `" + column.name +
                    "` has non-numeric categories. Supply an explicit numeric mapping before converting it to Numeric.";
            }
            return false;
        }
        for (const std::string &value : displayed) {
            if (DataCellIsMissing(value)) converted.push_back("NA");
            else {
                const auto found = mapping.find(value);
                if (found == mapping.end()) {
                    if (message) *message = "Category `" + value + "` is absent from the numeric mapping.";
                    return false;
                }
                converted.push_back(found->second);
            }
        }
    }

    auto convertStored = [&](const std::string &stored, std::string &out) {
        const std::string value = semanticValue(stored);
        if (DataCellIsMissing(value)) {
            out = "NA";
            return true;
        }
        if (allNumeric || mostlyNumeric) {
            double parsed = NAN;
            if (!ParseDataCellDouble(value, parsed)) {
                if (mostlyNumeric) {
                    out = "NA";
                    return true;
                }
                if (message) *message = "An imputed value of `" + column.name +
                    "` is not numeric: `" + value + "`.";
                return false;
            }
            std::ostringstream normalized;
            normalized << std::setprecision(17) << parsed;
            out = normalized.str();
            return true;
        }
        auto found = mapping.find(value);
        if (found == mapping.end()) {
            if (message) *message = "An imputed value of `" + column.name +
                "` is absent from the numeric mapping: `" + value + "`.";
            return false;
        }
        out = found->second;
        return true;
    };

    auto originalValues = column.imputationOriginalValues;
    auto imputationValues = column.imputationValues;
    auto originalSparse = column.imputationOriginalSparse;
    auto imputationSparse = column.imputationValuesSparse;
    for (std::string &value : originalValues) {
        std::string next;
        if (!convertStored(value, next)) return false;
        value = std::move(next);
    }
    for (auto &version : imputationValues) {
        for (std::string &value : version) {
            std::string next;
            if (!convertStored(value, next)) return false;
            value = std::move(next);
        }
    }
    for (auto &[row, value] : originalSparse) {
        (void)row;
        std::string next;
        if (!convertStored(value, next)) return false;
        value = std::move(next);
    }
    for (auto &version : imputationSparse) {
        for (auto &[row, value] : version) {
            (void)row;
            std::string next;
            if (!convertStored(value, next)) return false;
            value = std::move(next);
        }
    }

    if (allNumeric && sourceType != "character") {
        for (const std::string &level : sourceLevels) {
            double parsed = NAN;
            if (!ParseDataCellDouble(level, parsed)) continue;
            std::ostringstream normalized;
            normalized << std::setprecision(17) << parsed;
            mapping[level] = normalized.str();
        }
    }
    const bool reversibleCategoryLabels = sourceType == "factor" || sourceType == "ordered" ||
        sourceType == "logical";
    column.values = std::move(converted);
    column.displayValues.clear();
    // Keep the original two labels as dormant metadata. Numeric rendering and
    // analysis still use the visible 0/1 values; if the user changes the type
    // back to factor, EncodeCategoricalColumn can restore the exact labels.
    column.definedLevels.clear();
    column.reversibleFactorLevels = reversibleCategoryLabels
        ? sourceLevels : std::vector<std::string>{};
    column.numericMapping = reversibleCategoryLabels ? mapping : std::map<std::string, std::string>{};
    column.reversibleCategoryType = reversibleCategoryLabels ? sourceType : std::string{};
    column.imputationOriginalValues = std::move(originalValues);
    column.imputationValues = std::move(imputationValues);
    column.imputationOriginalSparse = std::move(originalSparse);
    column.imputationValuesSparse = std::move(imputationSparse);
    return true;
}

double QuantileValue(std::vector<double> values, double probability)
{
    values.erase(std::remove_if(values.begin(), values.end(), [](double value) {
        return !std::isfinite(value);
    }), values.end());
    if (values.empty()) {
        return NAN;
    }
    std::sort(values.begin(), values.end());
    if (values.size() == 1) {
        return values.front();
    }
    probability = std::max(0.0, std::min(1.0, probability));
    double position = probability * static_cast<double>(values.size() - 1);
    std::size_t lower = static_cast<std::size_t>(std::floor(position));
    std::size_t upper = std::min(values.size() - 1, lower + 1);
    double fraction = position - static_cast<double>(lower);
    return values[lower] + (values[upper] - values[lower]) * fraction;
}

} // namespace

bool DataCellIsMissing(const std::string &value)
{
    return value.empty() || value == "NA" || value == "NaN";
}

bool ParseDataCellDouble(const std::string &value, double &out)
{
    if (DataCellIsMissing(value)) {
        return false;
    }
    std::string normalized = TrimCopy(value);
    // Imported survey files commonly contain decimal commas even when the
    // current C locale expects a decimal point.  Accept one comma as the
    // decimal mark (including entries such as "15,") without weakening the
    // full-string validation below.
    if (normalized.find('.') == std::string::npos &&
        std::count(normalized.begin(), normalized.end(), ',') == 1) {
        std::replace(normalized.begin(), normalized.end(), ',', '.');
    }
    char *endptr = nullptr;
    out = std::strtod(normalized.c_str(), &endptr);
    while (endptr && *endptr && std::isspace(static_cast<unsigned char>(*endptr))) {
        ++endptr;
    }
    return endptr && endptr != normalized.c_str() && *endptr == '\0' && std::isfinite(out);
}

double ParseOptionalDataCellDouble(const std::string &value)
{
    double parsed = NAN;
    return ParseDataCellDouble(value, parsed) ? parsed : NAN;
}

int ParseOptionalDataCellInt(const std::string &value)
{
    double parsed = NAN;
    if (!ParseDataCellDouble(value, parsed)) {
        return 0;
    }
    return static_cast<int>(std::llround(parsed));
}

std::vector<int> ParseRowIdList(const std::string &text)
{
    std::vector<int> rows;
    std::string current;
    std::istringstream input(text);
    while (std::getline(input, current, ',')) {
        if (current.empty()) {
            continue;
        }
        int row = std::atoi(current.c_str());
        if (row > 0) {
            rows.push_back(row);
        }
    }
    return rows;
}

std::string CsvEscape(const std::string &value)
{
    bool quote = value.find_first_of(",\"\n\r") != std::string::npos;
    std::string out;
    for (char ch : value) {
        if (ch == '"') {
            out += "\"\"";
        } else {
            out += ch;
        }
    }
    return quote ? "\"" + out + "\"" : out;
}

bool WriteDataFrameCSV(std::ostream &out,
                       const DataFrameModel &df,
                       const std::vector<int> &includeRows)
{
    out << "..rlispstat_row_id";
    for (std::size_t c = 0; c < df.columns.size(); ++c) {
        out << ",";
        out << CsvEscape(df.columns[c].name);
    }
    out << "\n";
    std::set<int> include(includeRows.begin(), includeRows.end());
    for (int row = 0; row < df.rows; ++row) {
        if (!include.empty() && include.find(row + 1) == include.end()) {
            continue;
        }
        out << (row + 1);
        for (std::size_t c = 0; c < df.columns.size(); ++c) {
            out << ",";
            const DataColumn &col = df.columns[c];
            const std::string type = NormalizeVariableType(col.type);
            const bool labelledFactor = (type == "factor" || type == "ordered") &&
                col.displayValues.size() == col.values.size();
            std::string value = row < static_cast<int>(col.values.size())
                ? (labelledFactor ? col.displayValues[static_cast<std::size_t>(row)]
                                  : col.values[static_cast<std::size_t>(row)])
                : "NA";
            out << CsvEscape(value);
        }
        out << "\n";
    }
    return true;
}

namespace {

std::string UserFacingExportValue(const DataFrameModel &df,
                                  const DataColumn &col,
                                  std::size_t row,
                                  int imputationIndex)
{
    const bool imputed = DataFrameCellIsImputed(df, col, row);
    std::string value;
    if (imputationIndex < 0 && imputed) {
        value = OriginalImputationValueForCell(col, row);
    } else if (imputationIndex >= 0 && imputed) {
        value = ImputationVersionValueForCell(
            col, row, static_cast<std::size_t>(imputationIndex));
    } else {
        value = row < col.values.size() ? col.values[row] : "NA";
    }

    const std::string type = NormalizeVariableType(col.type);
    if ((type == "factor" || type == "ordered") && !DataCellIsMissing(value)) {
        // Imported labelled factors retain their user-visible labels separately
        // from the integer codes used by the R transport.
        if (!imputed && col.displayValues.size() == col.values.size() &&
            row < col.displayValues.size()) {
            return col.displayValues[row];
        }
        if (!col.definedLevels.empty() && col.displayValues.size() == col.values.size()) {
            char *end = nullptr;
            long code = std::strtol(value.c_str(), &end, 10);
            if (end && *end == '\0' && code >= 1 &&
                static_cast<std::size_t>(code) <= col.definedLevels.size()) {
                return col.definedLevels[static_cast<std::size_t>(code - 1)];
            }
        }
    }
    return value;
}

void WriteUserFacingExportHeader(std::ostream &out,
                                 const DataFrameModel &df,
                                 bool multipleImputation)
{
    if (multipleImputation) out << ".imp,.id";
    for (std::size_t c = 0; c < df.columns.size(); ++c) {
        if (multipleImputation || c > 0) out << ",";
        out << CsvEscape(df.columns[c].name);
    }
    out << "\n";
}

void WriteUserFacingExportRow(std::ostream &out,
                              const DataFrameModel &df,
                              int row,
                              int imputationIndex)
{
    const bool multipleImputation = imputationIndex >= -1 &&
        df.datasetType == "multiple_imputation" && df.imputationCount > 0;
    if (multipleImputation) {
        out << (imputationIndex + 1) << "," << (row + 1);
    }
    for (std::size_t c = 0; c < df.columns.size(); ++c) {
        if (multipleImputation || c > 0) out << ",";
        out << CsvEscape(UserFacingExportValue(
            df, df.columns[c], static_cast<std::size_t>(row), imputationIndex));
    }
    out << "\n";
}

} // namespace

bool WriteDataExportCSV(std::ostream &out, const DataFrameModel &df)
{
    const bool multipleImputation = df.datasetType == "multiple_imputation" &&
        df.imputationCount > 0;
    WriteUserFacingExportHeader(out, df, multipleImputation);
    if (multipleImputation) {
        // -1 maps to the conventional .imp = 0 original incomplete dataset.
        for (int imputationIndex = -1;
             imputationIndex < df.imputationCount;
             ++imputationIndex) {
            for (int row = 0; row < df.rows; ++row) {
                WriteUserFacingExportRow(out, df, row, imputationIndex);
            }
        }
    } else {
        for (int row = 0; row < df.rows; ++row) {
            WriteUserFacingExportRow(out, df, row, -2);
        }
    }
    return static_cast<bool>(out);
}

bool WriteDataExportMetadataCSV(std::ostream &out, const DataFrameModel &df)
{
    out << "variable,type,level_index,level\n";
    for (const DataColumn &column : df.columns) {
        const std::string type = NormalizeVariableType(column.type);
        out << CsvEscape(column.name) << "," << CsvEscape(type) << ",0,\n";
        if (type != "factor" && type != "ordered") continue;
        for (std::size_t index = 0; index < column.definedLevels.size(); ++index) {
            out << CsvEscape(column.name) << "," << CsvEscape(type) << ","
                << (index + 1) << "," << CsvEscape(column.definedLevels[index]) << "\n";
        }
    }
    return static_cast<bool>(out);
}

DataFrameModel SubsetDataFrame(const DataFrameModel &source,
                               const std::vector<int> &originalRowIds,
                               const std::string &newGroup)
{
    std::vector<std::size_t> indices;
    std::set<int> seen;
    for (int row : originalRowIds) {
        if (row <= 0 || row > source.rows || !seen.insert(row).second) continue;
        indices.push_back(static_cast<std::size_t>(row - 1));
    }

    auto subsetStrings = [&](const std::vector<std::string> &values) {
        std::vector<std::string> result;
        result.reserve(indices.size());
        for (std::size_t index : indices) {
            result.push_back(index < values.size() ? values[index] : std::string());
        }
        return result;
    };
    auto subsetBools = [&](const std::vector<bool> &values) {
        std::vector<bool> result;
        result.reserve(indices.size());
        for (std::size_t index : indices) {
            result.push_back(index < values.size() ? values[index] : false);
        }
        return result;
    };
    auto subsetSparse = [&](const std::map<std::size_t, std::string> &values) {
        std::map<std::size_t, std::string> result;
        for (std::size_t target = 0; target < indices.size(); ++target) {
            auto found = values.find(indices[target]);
            if (found != values.end()) result[target] = found->second;
        }
        return result;
    };

    DataFrameModel subset = source;
    subset.group = newGroup;
    subset.rows = static_cast<int>(indices.size());
    if (source.datasetType != "multiple_imputation") {
        subset.sourceDatasetId = source.group;
    }
    for (DataColumn &column : subset.columns) {
        column.values = subsetStrings(column.values);
        if (!column.displayValues.empty()) {
            column.displayValues = subsetStrings(column.displayValues);
        }
        if (!column.imputedMissing.empty()) {
            column.imputedMissing = subsetBools(column.imputedMissing);
        }
        if (!column.imputationOriginalValues.empty()) {
            column.imputationOriginalValues = subsetStrings(column.imputationOriginalValues);
        }
        for (std::vector<std::string> &version : column.imputationValues) {
            version = subsetStrings(version);
        }
        column.imputationOriginalSparse = subsetSparse(column.imputationOriginalSparse);
        for (std::map<std::size_t, std::string> &version : column.imputationValuesSparse) {
            version = subsetSparse(version);
        }
    }
    subset.stableRowIds.clear();
    subset.stableRowIds.reserve(indices.size());
    for (std::size_t index : indices) {
        if (index < source.stableRowIds.size()) subset.stableRowIds.push_back(source.stableRowIds[index]);
        else subset.stableRowIds.push_back(source.group + ":row:" + std::to_string(index + 1));
    }
    subset.dataVersion = 1;
    subset.provenance = DataProvenance{};
    EnsureDataFrameProvenance(subset, RCodeOrigin::Recorded, {},
                              "Subset created in LinkEDA.");
    TransformationStep step;
    step.label = "Subset rows from " + source.group;
    step.origin = RCodeOrigin::Recorded;
    step.parentVersionKeys = {source.provenance.currentVersion.storageKey};
    step.stableRowIds = subset.stableRowIds;
    step.rCode = "source_data <- LinkEDA::ls_get_data_version(" +
        ProvenanceRStringLiteral(source.group) + ", version = " +
        std::to_string(std::max<std::uint64_t>(1, source.dataVersion)) + ")\n" +
        "data <- LinkEDA::ls_select_rows_by_id(source_data, " +
        [&] { std::ostringstream out; out << "c("; for (std::size_t i = 0; i < subset.stableRowIds.size(); ++i) { if (i) out << ", "; out << ProvenanceRStringLiteral(subset.stableRowIds[i]); } out << ")"; return out.str(); }() + ")";
    RecordDataFrameTransformation(subset, std::move(step));
    return subset;
}

DataFrameModel SubsetDataFrameColumns(const DataFrameModel &source,
                                     const std::vector<std::string> &columnNames,
                                     const std::string &newGroup)
{
    DataFrameModel subset = source;
    subset.group = newGroup;
    subset.columns.clear();
    const std::set<std::string> wanted(columnNames.begin(), columnNames.end());
    for (const DataColumn &column : source.columns) {
        if (wanted.count(column.name)) subset.columns.push_back(column);
    }
    if (source.datasetType != "multiple_imputation") {
        subset.sourceDatasetId = source.group;
    } else {
        // The original mids process belongs to the full set of variables. R
        // retains the selected completed datasets, but cannot attribute the
        // original imputation diagnostics to this projected dataset.
        subset.imputationProcess.clear();
        subset.imputationId.clear();
    }
    subset.dataVersion = 1;
    subset.syncSessionToken.clear();
    subset.verificationPreparedRds.clear();
    subset.provenance = DataProvenance{};
    EnsureDataFrameProvenance(subset, RCodeOrigin::Recorded, {},
                              "Variable subset created in LinkEDA.");
    TransformationStep step;
    step.label = "Subset variables from " + source.group;
    step.origin = RCodeOrigin::Recorded;
    step.parentVersionKeys = {source.provenance.currentVersion.storageKey};
    step.stableRowIds = subset.stableRowIds;
    std::ostringstream names;
    names << "c(";
    for (std::size_t i = 0; i < subset.columns.size(); ++i) {
        if (i) names << ", ";
        names << ProvenanceRStringLiteral(subset.columns[i].name);
    }
    names << ")";
    step.rCode = "source_data <- LinkEDA::ls_get_data_version(" +
        ProvenanceRStringLiteral(source.group) + ", version = " +
        std::to_string(std::max<std::uint64_t>(1, source.dataVersion)) + ")\n" +
        "data <- source_data[, " + names.str() + ", drop = FALSE]";
    RecordDataFrameTransformation(subset, std::move(step));
    return subset;
}

std::string NativeImportRScript()
{
    return R"RLSIMPORT(
args <- commandArgs(TRUE)
if (length(args) < 3L) stop("Expected path, dataset name, and output path.", call. = FALSE)
path <- args[[1L]]
dataset_name <- args[[2L]]
output <- args[[3L]]

fail <- function(...) stop(sprintf(...), call. = FALSE)
clean_names <- function(names) {
  names <- as.character(names)
  names[is.na(names) | !nzchar(names)] <- "V"
  names <- gsub("[\r\n\t|]+", " ", names)
  names <- trimws(names)
  names[!nzchar(names)] <- "V"
  make.unique(names)
}
check_path <- function(path) {
  if (!file.exists(path)) fail("Cannot import data because file does not exist: %s", path)
  if (file.access(path, 4) != 0L) fail("Cannot import data because file is not readable: %s", path)
  normalizePath(path, mustWork = TRUE)
}
read_import <- function(path) {
  ext <- tolower(tools::file_ext(path))
  source <- NULL
  data <- switch(
    ext,
    csv = {
      source <- "CSV file"
      utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                      na.strings = c("NA", ""))
    },
    tsv = ,
    txt = {
      source <- "Delimited text file"
      utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE,
                        na.strings = c("NA", ""))
    },
    sav = ,
    zsav = {
      if (!requireNamespace("haven", quietly = TRUE)) fail("Cannot import SPSS file because package 'haven' is not installed.")
      source <- "SPSS file"
      haven::read_sav(path)
    },
    dta = {
      if (!requireNamespace("haven", quietly = TRUE)) fail("Cannot import Stata file because package 'haven' is not installed.")
      source <- "Stata file"
      haven::read_dta(path)
    },
    sas7bdat = {
      if (!requireNamespace("haven", quietly = TRUE)) fail("Cannot import SAS file because package 'haven' is not installed.")
      source <- "SAS file"
      haven::read_sas(path)
    },
    xpt = {
      if (!requireNamespace("haven", quietly = TRUE)) fail("Cannot import SAS transport file because package 'haven' is not installed.")
      source <- "SAS transport file"
      haven::read_xpt(path)
    },
    xls = ,
    xlsx = {
      if (!requireNamespace("readxl", quietly = TRUE)) fail("Cannot import Excel file because package 'readxl' is not installed.")
      sheets <- readxl::excel_sheets(path)
      if (!length(sheets)) fail("Cannot import Excel file because no readable sheets were found.")
      source <- paste0("Excel file: ", sheets[[1L]])
      readxl::read_excel(path, sheet = sheets[[1L]], .name_repair = "unique")
    },
    rds = {
      source <- "RDS file"
      readRDS(path)
    },
    rda = ,
    rdata = {
      env <- new.env(parent = emptyenv())
      loaded <- load(path, envir = env)
      mids_names <- loaded[vapply(loaded, function(object_name) inherits(get(object_name, envir = env), "mids"), logical(1L))]
      if (length(mids_names)) {
        sizes <- vapply(mids_names, function(object_name) {
          object <- get(object_name, envir = env)
          nrow(object$data) * max(1L, ncol(object$data))
        }, numeric(1L))
        selected <- mids_names[[which.max(sizes)]]
        source <- paste0("mice mids R data file: ", selected)
        return(list(data = get(selected, envir = env), source = source))
      }
      data_names <- loaded[vapply(loaded, function(object_name) is.data.frame(get(object_name, envir = env)), logical(1L))]
      if (!length(data_names)) {
        if (requireNamespace("LinkEDA",quietly=TRUE) && any(vapply(loaded,function(name)
            identical(LinkEDA:::.rls_classify_r_import_object(get(name,envir=env)),"completed_dataset_list"),logical(1L))))
          stop(LinkEDA:::.rls_mi_import_requirement(),call.=FALSE)
        fail("The selected R data file did not contain a data frame.")
      }
      sizes <- vapply(data_names, function(object_name) {
        data <- get(object_name, envir = env)
        nrow(data) * max(1L, ncol(data))
      }, numeric(1L))
      selected <- data_names[[which.max(sizes)]]
      source <- paste0("R data file: ", selected)
      get(selected, envir = env)
    },
    fail("Unsupported data file. Use .csv, .txt, .tsv, .sav, .zsav, .dta, .sas7bdat, .xpt, .xlsx, .xls, .rds, .rda, or .RData.")
  )
  list(data = data, source = source)
}
variable_type <- function(x) {
  if (is.ordered(x)) "ordered"
  else if (is.factor(x)) "factor"
  else if (is.character(x)) "character"
  else if (is.logical(x)) "logical"
  else if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) "datetime"
  else if (is.numeric(x)) "numeric"
  else "other"
}
imported_variable_type <- function(x, name = "") {
  type <- variable_type(x)
  if (identical(type, "logical")) return("factor")
  if (identical(type, "character")) {
    return("character")
  }
  if (!identical(type, "numeric")) return(type)
  if (length(attr(x, "labels", exact = TRUE))) return("factor")
  "numeric"
}
variable_label <- function(x) {
  label <- attr(x, "label", exact = TRUE)
  if (is.null(label) || !length(label) || is.na(label[[1L]])) return("")
  as.character(label[[1L]])
}
variable_decimals <- function(x) {
  format <- attr(x, "format.spss", exact = TRUE)
  if (!is.null(format) && length(format) && !is.na(format[[1L]])) {
    hit <- regexec("\\.([0-9]+)", as.character(format[[1L]]))
    parts <- regmatches(as.character(format[[1L]]), hit)[[1L]]
    if (length(parts) >= 2L) return(as.integer(parts[[2L]]))
  }
  if (is.integer(x) || is.logical(x)) return(0L)
  -1L
}
metadata_value <- function(value) {
  value <- as.character(value)
  if (!length(value) || is.na(value[[1L]])) return("")
  gsub("[\r\n\t]+", " ", value[[1L]])
}
variable_payload <- function(data, types) {
  numeric_names <- names(types)[types == "numeric"]
  lines <- c("VARS", as.character(length(numeric_names)))
  for (name in numeric_names) {
    values <- as.double(data[[name]])
    value_lines <- ifelse(is.na(values) | !is.finite(values), "NA", sprintf("%.17g", values))
    lines <- c(lines, name, as.character(length(values)), value_lines)
  }
  lines <- c(lines, "VARMETA", as.character(length(names(data))))
  for (name in names(data)) {
    lines <- c(lines, name, types[[name]])
  }
  lines
}
dataframe_payload <- function(data, types, max_cell_chars = NULL) {
  clean_values <- function(values) {
    values[is.na(values)] <- "NA"
    LinkEDA:::.rls_encode_data_value(values)
  }
  raw_values <- function(x) {
    if (!is.null(attr(x, "labels", exact = TRUE))) {
      raw <- unclass(x)
      attributes(raw) <- NULL
      return(as.character(raw))
    }
    as.character(x)
  }
  labelled_values <- function(x) {
    labels <- attr(x, "labels", exact = TRUE)
    raw <- if (!is.null(labels)) {
      tmp <- unclass(x)
      attributes(tmp) <- NULL
      tmp
    } else {
      x
    }
    out <- as.character(raw)
    if (!is.null(labels) && length(labels)) {
      label_values <- unclass(labels)
      attributes(label_values) <- NULL
      label_names <- names(labels)
      if (is.null(label_names)) label_names <- rep("", length(labels))
      if (length(label_names) < length(labels)) {
        label_names <- c(label_names, rep("", length(labels) - length(label_names)))
      }
      for (i in seq_along(label_values)) {
        label <- label_names[[i]]
        if (!length(label) || is.na(label) || !nzchar(label)) label <- as.character(label_values[[i]])
        out[!is.na(raw) & raw == label_values[[i]]] <- label
      }
    }
    out
  }
  semantic_levels <- function(x, type) {
    if (!type %in% c("factor", "ordered", "logical")) return(character())
    if (is.factor(x)) return(as.character(levels(x)))
    labels <- attr(x, "labels", exact = TRUE)
    if (!is.null(labels) && length(labels)) {
      label_names <- names(labels)
      if (is.null(label_names)) label_names <- rep("", length(labels))
      raw <- unclass(labels)
      attributes(raw) <- NULL
      missing_names <- is.na(label_names) | !nzchar(label_names)
      label_names[missing_names] <- as.character(raw)[missing_names]
      return(unique(as.character(label_names)))
    }
    if (is.logical(x)) return(c("FALSE", "TRUE"))
    observed <- as.character(x[!is.na(x)])
    observed <- observed[nzchar(trimws(observed))]
    unique(observed)
  }
  lines <- c("DATAFRAME", as.character(nrow(data)), as.character(ncol(data)), "DATACELLS_PERCENT_V1")
  display <- list()
  for (name in names(data)) {
    values <- clean_values(raw_values(data[[name]]))
    shown <- clean_values(labelled_values(data[[name]]))
    if (!identical(values, shown)) display[[name]] <- shown
    lines <- c(lines, name, types[[name]], values)
  }
  if (length(display)) {
    lines <- c(lines, "DATADISPLAY", as.character(length(display)))
    for (name in names(display)) {
      lines <- c(lines, name, display[[name]])
    }
  }
  level_columns <- names(types)[types %in% c("factor", "ordered", "logical")]
  if (length(level_columns)) {
    lines <- c(lines, "DATLEVELS", as.character(length(level_columns)))
    for (name in level_columns) {
      defined <- clean_values(semantic_levels(data[[name]], types[[name]]))
      lines <- c(lines, name, as.character(length(defined)), defined)
    }
  }
  lines <- c(lines, "DATAMETA", as.character(length(names(data))))
  for (name in names(data)) {
    lines <- c(lines, name, name, metadata_value(variable_label(data[[name]])),
               as.character(variable_decimals(data[[name]])))
  }
  lines <- c(lines, "DATATYPEMETA", as.character(length(names(data))))
  for (name in names(data)) {
    type <- types[[name]]
    if (identical(type, "logical")) type <- "factor"
    defined <- semantic_levels(data[[name]], type)
    storage <- paste(class(data[[name]]), collapse = ", ")
    binary <- type %in% c("factor", "ordered") && length(defined) == 2L
    lines <- c(lines, name, type, metadata_value(storage), if (binary) "1" else "0", "",
               as.character(length(defined)), clean_values(defined), "0")
  }
  lines
}
imputation_payload <- function(original, completed, mask, impute_id, source_id,
                               max_cell_chars = NULL) {
  clean_values <- function(values) {
    values <- as.character(values)
    values[is.na(values)] <- "NA"
    LinkEDA:::.rls_encode_data_value(values)
  }
  imputed_names <- names(original)[vapply(names(original), function(name) {
    any(mask[[name]])
  }, logical(1L))]
  lines <- c(
    "IMPUTATION_SPARSE", "multiple_imputation", impute_id, source_id,
    as.character(length(completed)), "1", "version", as.character(length(imputed_names))
  )
  for (name in imputed_names) {
    missing_rows <- which(mask[[name]])
    lines <- c(
      lines, name, as.character(length(missing_rows)), as.character(missing_rows),
      clean_values(original[[name]][missing_rows])
    )
    for (version in seq_along(completed)) {
      lines <- c(lines, clean_values(completed[[version]][[name]][missing_rows]))
    }
  }
  lines
}

tryCatch({
  path <- check_path(path)
  imported <- read_import(path)
  if (requireNamespace("LinkEDA",quietly=TRUE)) {
    if (identical(LinkEDA:::.rls_classify_r_import_object(imported$data),"completed_dataset_list"))
      stop(LinkEDA:::.rls_mi_import_requirement(),call.=FALSE)
    notice <- LinkEDA:::.rls_mi_stacked_import_notice(imported$data)
    if (length(notice) && nzchar(notice)) message(notice)
  }
  if (inherits(imported$data, "mids")) {
    if (!requireNamespace("mice", quietly = TRUE)) {
      fail("The RDS file contains a mice mids object, but the 'mice' package is not installed.")
    }
    mids <- imported$data
    m <- suppressWarnings(as.integer(mids$m))
    if (length(m) != 1L || !is.finite(m) || m < 1L || !is.data.frame(mids$data)) {
      fail("The selected file contains an invalid mice mids object.")
    }
    data <- as.data.frame(mids$data, stringsAsFactors = FALSE)
    original_names <- names(data)
    completed <- lapply(seq_len(m), function(version) {
      value <- as.data.frame(mice::complete(mids, action = version), stringsAsFactors = FALSE)
      if (!anyDuplicated(original_names) && all(original_names %in% names(value))) value <- value[original_names]
      if (nrow(value) != nrow(data) || ncol(value) != ncol(data)) {
        fail("Completed imputation %d does not match the mice source data.", version)
      }
      value
    })
    clean <- clean_names(original_names)
    names(data) <- clean
    for (version in seq_along(completed)) names(completed[[version]]) <- clean
    if (!nrow(data) || !ncol(data)) fail("The mice mids object contained an empty source data frame.")
    mask <- as.data.frame(lapply(data, is.na), stringsAsFactors = FALSE)
    current <- completed[[1L]]
    types <- vapply(names(data), function(name) imported_variable_type(data[[name]], name), character(1L))
    payload <- c(
      "REGISTER_DATASET", dataset_name, variable_payload(current, types),
      dataframe_payload(current, types),
      imputation_payload(
        data, completed, mask, paste0("imported_", dataset_name),
        paste0("mice:", basename(path))
      )
    )
    payload <- c(payload, "IMPUTATION_PROCESS_V1", LinkEDA:::.rls_mi_encode_process(mids,
      data.frame(original_name=original_names,variable_name=clean)))
    writeLines(payload, output, useBytes = TRUE)
    quit(status = 0L)
  }
  if (!is.data.frame(imported$data)) fail("The selected file was read successfully, but it did not contain a data frame or a mice mids object.")
  data <- as.data.frame(imported$data, stringsAsFactors = FALSE)
  if (!nrow(data) || !ncol(data)) fail("The selected file was read successfully, but it contained an empty data frame.")
  names(data) <- clean_names(names(data))
  types <- vapply(names(data), function(name) imported_variable_type(data[[name]], name), character(1L))
  payload <- c("REGISTER_DATASET", dataset_name, variable_payload(data, types), dataframe_payload(data, types))
  writeLines(payload, output, useBytes = TRUE)
}, error = function(e) {
  message(conditionMessage(e))
  quit(status = 1L)
})
)RLSIMPORT";
}

std::string NativeMiceImputationRScript()
{
    return R"RLSMICE(
args <- commandArgs(TRUE)
if (length(args) < 9L) stop("Expected input, output, dataset name, impute variables, predictors, methods, m, maxit, and seed.", call. = FALSE)
scalar_text <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[[1L]])) return("")
  as.character(x[[1L]])
}
has_text <- function(x) {
  length(x) == 1L && !is.na(x) && nzchar(x)
}
input <- args[[1L]]
output <- args[[2L]]
dataset_name <- args[[3L]]
impute_text <- args[[4L]]
predictor_text <- args[[5L]]
method_text <- args[[6L]]
m <- as.integer(args[[7L]])
maxit <- as.integer(args[[8L]])
seed_text <- scalar_text(args[[9L]])
seed <- if (has_text(seed_text)) as.integer(seed_text) else NULL

missing_msg <- 'Multiple imputation requires the mice package.\nInstall it with install.packages("mice") and try again.'
if (!requireNamespace("mice", quietly = TRUE)) stop(missing_msg, call. = FALSE)

fail <- function(...) stop(sprintf(...), call. = FALSE)
stage <- "initializing"
clean_values <- function(values, max_cell_chars = NULL) {
  values <- as.character(values)
  values[is.na(values)] <- "NA"
  LinkEDA:::.rls_encode_data_value(values)
}
split_names <- function(text) {
  text <- trimws(scalar_text(text))
  if (!has_text(text)) return(character())
  sep <- if (length(grep("\t", text, fixed = TRUE))) "\t" else ","
  out <- trimws(strsplit(text, sep, fixed = TRUE)[[1L]])
  out[nzchar(out)]
}
split_fields <- function(text) {
  text <- trimws(scalar_text(text))
  if (!has_text(text)) return(character())
  sep <- if (length(grep("\t", text, fixed = TRUE))) "\t" else ","
  out <- trimws(strsplit(text, sep, fixed = TRUE)[[1L]])
  out[nzchar(out)]
}
parse_methods <- function(text) {
  fields <- split_fields(text)
  out <- character()
  for (field in fields) {
    pair <- strsplit(field, "=", fixed = TRUE)[[1L]]
    left <- if (length(pair) >= 1L) trimws(pair[[1L]]) else ""
    right <- if (length(pair) >= 2L) trimws(pair[[2L]]) else ""
    if (length(pair) != 2L || !has_text(left) || !has_text(right)) {
      fail("Malformed imputation method specification.")
    }
    out[[left]] <- right
  }
  out
}
is_missing_text <- function(x) is.na(x) | x %in% c("", "NA", "NaN")
variable_type <- function(x) {
  if (is.numeric(x)) "numeric"
  else if (is.ordered(x)) "ordered"
  else if (is.factor(x)) "factor"
  else if (is.character(x)) "character"
  else if (is.logical(x)) "logical"
  else "other"
}
read_payload <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (length(lines) >= 5L && identical(lines[[5L]], "DATACELLS_PERCENT_V1")) {
    lines <- lines[-5L]
    escaped <- which(grepl("%", lines, fixed = TRUE))
    if (length(escaped)) lines[escaped] <- utils::URLdecode(lines[escaped])
  }
  i <- 1L
  next_line <- function() {
    if (i > length(lines)) fail("Malformed imputation payload.")
    value <- lines[[i]]
    i <<- i + 1L
    value
  }
  if (!identical(next_line(), "DATASET")) fail("Malformed imputation payload.")
  source <- next_line()
  rows <- as.integer(next_line())
  cols <- as.integer(next_line())
  data <- list()
  types <- character()
  for (ci in seq_len(cols)) {
    name <- next_line()
    type <- next_line()
    values <- vapply(seq_len(rows), function(ri) next_line(), character(1L))
    miss <- is_missing_text(values)
    if (identical(type, "numeric")) {
      x <- suppressWarnings(as.numeric(values))
      x[miss] <- NA_real_
    } else if (identical(type, "logical")) {
      text <- tolower(values)
      x <- rep(NA, length(values))
      x[text %in% c("true", "t", "1", "yes", "y")] <- TRUE
      x[text %in% c("false", "f", "0", "no", "n")] <- FALSE
    } else if (identical(type, "factor") || identical(type, "ordered")) {
      x <- values
      x[miss] <- NA_character_
      x <- factor(x, ordered = identical(type, "ordered"))
    } else {
      x <- values
      x[miss] <- NA_character_
    }
    data[[name]] <- x
    types[[name]] <- type
  }
  list(source = source, data = as.data.frame(data, check.names = FALSE), types = types)
}
is_id_like <- function(x, name) {
  lname <- tolower(scalar_text(name))
  if (length(grep("(^id$|_id$|^id_|identifier|subject|case|etiqueta|label)", lname))) return(TRUE)
  observed <- x[!is.na(x)]
  if (!length(observed)) return(FALSE)
  if (is.numeric(observed) || is.integer(observed)) {
    sorted <- sort(unique(observed))
    return(length(sorted) == length(observed) &&
           all(abs(sorted - round(sorted)) < .Machine$double.eps^0.5) &&
           all(diff(sorted) == 1))
  }
  length(unique(observed)) == length(observed) && length(observed) >= 0.9 * length(x)
}
default_method <- function(data, types, variable) {
  x <- data[[variable]]
  type <- types[[variable]]
  observed <- x[!is.na(x)]
  if (!length(observed)) return(NA_character_)
  if (identical(type, "numeric")) return("pmm")
  if (identical(type, "logical")) return("logreg")
  if (identical(type, "character")) {
    return(if (nlevels(factor(observed)) <= 2L) "logreg" else "polyreg")
  }
  if (identical(type, "ordered")) {
    n <- nlevels(droplevels(x))
    if (n <= 2L) "logreg" else "polr"
  } else if (identical(type, "factor")) {
    n <- nlevels(droplevels(x))
    if (n <= 2L) "logreg" else if (is.ordered(x)) "polr" else "polyreg"
  } else {
    NA_character_
  }
}
variable_payload <- function(data, types) {
  numeric_names <- names(data)[vapply(names(data), function(nm) identical(types[[nm]], "numeric"), logical(1L))]
  lines <- c("VARS", as.character(length(numeric_names)))
  for (name in numeric_names) {
    values <- suppressWarnings(as.double(data[[name]]))
    value_lines <- ifelse(is.na(values) | !is.finite(values), "NA", sprintf("%.17g", values))
    lines <- c(lines, name, as.character(length(values)), value_lines)
  }
  lines <- c(lines, "VARMETA", as.character(length(names(data))))
  for (name in names(data)) lines <- c(lines, name, types[[name]])
  lines
}
dataframe_payload <- function(current, original, completed, mask, types, impute_id, source_id) {
  lines <- c("DATAFRAME", as.character(nrow(current)), as.character(ncol(current)), "DATACELLS_PERCENT_V1")
  for (name in names(current)) {
    lines <- c(lines, name, types[[name]], clean_values(current[[name]]))
  }
  lines <- c(lines, "DATATYPEMETA", as.character(length(names(current))))
  for (name in names(current)) {
    type <- types[[name]]
    if (identical(type, "logical")) type <- "factor"
    levels <- if (type %in% c("factor", "ordered")) {
      if (is.factor(current[[name]])) as.character(levels(current[[name]]))
      else unique(as.character(current[[name]][!is.na(current[[name]])]))
    } else character()
    storage <- gsub("[\r\n\t|]", " ", paste(class(current[[name]]), collapse = ", "))
    binary <- type %in% c("factor", "ordered") && length(levels) == 2L
    lines <- c(lines, name, type, storage, if (binary) "1" else "0", "",
               as.character(length(levels)), clean_values(levels), "0")
  }
  imputed_names <- names(current)[vapply(names(current), function(name) {
    name %in% names(mask) && any(mask[[name]])
  }, logical(1L))]
  lines <- c(lines, "IMPUTATION_SPARSE", "multiple_imputation", impute_id, source_id,
             as.character(length(completed)), "1", "version", as.character(length(imputed_names)))
  for (name in imputed_names) {
    missing_rows <- which(mask[[name]])
    lines <- c(lines, name, as.character(length(missing_rows)), as.character(missing_rows),
               clean_values(original[[name]][missing_rows]))
    for (version in seq_along(completed)) {
      lines <- c(lines, clean_values(completed[[version]][[name]][missing_rows]))
    }
  }
  lines
}

tryCatch({
  stage <- "reading native data payload"
  payload <- read_payload(input)
  data <- payload$data
  types <- payload$types
  stage <- "parsing imputation selections"
  impute <- split_names(impute_text)
  predictors <- split_names(predictor_text)
  method_overrides <- parse_methods(method_text)
  supported <- names(data)[types %in% c("numeric", "factor", "ordered", "character", "logical")]
  stage <- "choosing variables"
  if (!length(impute)) {
    impute <- supported[vapply(supported, function(nm) any(is.na(data[[nm]])) && !all(is.na(data[[nm]])) && !is_id_like(data[[nm]], nm), logical(1L))]
  }
  if (!length(predictors)) {
    predictors <- supported[vapply(supported, function(nm) !all(is.na(data[[nm]])) && !is_id_like(data[[nm]], nm), logical(1L))]
  }
  predictors <- predictors[vapply(predictors, function(nm) {
    x <- data[[nm]]
    observed <- x[!is.na(x)]
    length(unique(observed)) > 1L && !is_id_like(x, nm)
  }, logical(1L))]
  unknown <- setdiff(c(impute, predictors), names(data))
  if (length(unknown)) fail("Variable '%s' was not found in the dataset.", unknown[[1L]])
  if (!length(impute)) fail("No variables with missing values were selected for imputation.")
  if (!length(predictors)) fail("No valid predictor variables were selected.")
  stage <- "validating imputation variables"
  for (nm in impute) {
    if (!any(is.na(data[[nm]]))) fail("Variable '%s' has no missing values to impute.", nm)
    if (all(is.na(data[[nm]]))) fail("Variable '%s' has all values missing and cannot be imputed safely.", nm)
  }
  stage <- "building mice method vector"
  methods <- rep("", ncol(data))
  names(methods) <- names(data)
  unknown_method_vars <- setdiff(names(method_overrides), impute)
  if (length(unknown_method_vars)) fail("Method was supplied for non-imputed variable '%s'.", unknown_method_vars[[1L]])
  for (nm in impute) {
    methods[[nm]] <- if (nm %in% names(method_overrides)) method_overrides[[nm]] else default_method(data, types, nm)
  }
  if (anyNA(methods[impute])) fail("Variable '%s' has an unsupported type for imputation.", impute[is.na(methods[impute])][[1L]])
  stage <- "building predictor matrix"
  predictor_matrix <- matrix(0, nrow = ncol(data), ncol = ncol(data), dimnames = list(names(data), names(data)))
  predictor_matrix[impute, predictors] <- 1
  diag(predictor_matrix) <- 0
  stage <- "preparing data for mice"
  data_for_mice <- data
  for (nm in names(data_for_mice)) {
    if (identical(types[[nm]], "character")) data_for_mice[[nm]] <- factor(data_for_mice[[nm]])
    if (identical(types[[nm]], "logical")) data_for_mice[[nm]] <- factor(data_for_mice[[nm]], levels = c(FALSE, TRUE))
  }
  stage <- "running mice"
  mice_args <- list(data = data_for_mice, m = m, maxit = maxit, method = methods,
                    predictorMatrix = predictor_matrix, printFlag = FALSE)
  if (!is.null(seed)) mice_args$seed <- seed
  mids <- do.call(mice::mice, mice_args)
  stage <- "collecting completed datasets"
  completed <- lapply(seq_len(m), function(i) as.data.frame(mice::complete(mids, i), stringsAsFactors = FALSE)[names(data)])
  mask <- as.data.frame(lapply(data, function(x) rep(FALSE, length(x))), stringsAsFactors = FALSE)
  for (nm in impute) mask[[nm]] <- is.na(data[[nm]])
  current <- completed[[1L]]
  stage <- "writing imputed dataset payload"
  out <- c("REGISTER_DATASET", dataset_name, variable_payload(current, types),
           dataframe_payload(current, data, completed, mask, types,
                             paste0("native_", dataset_name), payload$source))
  out <- c(out, "IMPUTATION_PROCESS_V1", LinkEDA:::.rls_mi_encode_process(mids))
  writeLines(out, output, useBytes = TRUE)
}, error = function(e) {
  tb <- paste(utils::capture.output(traceback(2)), collapse = "\n")
  msg <- conditionMessage(e)
  if (!nzchar(msg)) msg <- "Unknown imputation error."
  message(sprintf("Multiple imputation failed while %s: %s", stage, msg))
  if (nzchar(tb)) message(tb)
  quit(status = 1L)
})
)RLSMICE";
}

std::string NativePooledAnalysisRScript()
{
    return R"RLSANALYSIS(
args <- commandArgs(TRUE)
if (length(args) < 9L) stop("Expected input, output, mode, id, variables, group, response, terms, and scope.", call. = FALSE)
input <- args[[1L]]
output <- args[[2L]]
mode <- args[[3L]]
analysis_id <- args[[4L]]
variables_text <- args[[5L]]
group_text <- args[[6L]]
response_text <- args[[7L]]
terms_text <- args[[8L]]
scope_text <- args[[9L]]
model_spec_text <- if (length(args) >= 10L) args[[10L]] else ""

suppressPackageStartupMessages(library(LinkEDA))
`%||%` <- function(x, y) if (is.null(x)) y else x
scalar_text <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[[1L]])) return("")
  as.character(x[[1L]])
}
has_text <- function(x) {
  length(x) == 1L && !is.na(x) && nzchar(x)
}
split_names <- function(text) {
  text <- trimws(scalar_text(text))
  if (!has_text(text)) return(character())
  sep <- if (grepl("\t", text, fixed = TRUE)) "\t" else ","
  out <- trimws(strsplit(text, sep, fixed = TRUE)[[1L]])
  out[nzchar(out)]
}
decode_field <- function(text) {
  text <- scalar_text(text)
  if (!nzchar(text)) return("")
  chars <- strsplit(text, "", fixed = TRUE)[[1L]]
  out <- character()
  i <- 1L
  while (i <= length(chars)) {
    if (identical(chars[[i]], "%") && i + 2L <= length(chars) &&
        grepl("^[0-9A-Fa-f]{2}$", paste0(chars[[i + 1L]], chars[[i + 2L]]))) {
      out <- c(out, rawToChar(as.raw(strtoi(paste0(chars[[i + 1L]], chars[[i + 2L]]), 16L))))
      i <- i + 3L
    } else {
      out <- c(out, chars[[i]])
      i <- i + 1L
    }
  }
  paste0(out, collapse = "")
}
parse_model_specs <- function(text) {
  text <- scalar_text(text)
  if (!nzchar(text)) return(list())
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  specs <- list()
  for (line in lines[nzchar(lines)]) {
    fields <- strsplit(line, "|", fixed = TRUE)[[1L]]
    if (length(fields) < 3L) next
    terms <- character()
    terms_text <- fields[[3L]]
    if (nzchar(terms_text)) {
      terms <- vapply(strsplit(terms_text, "\t", fixed = TRUE)[[1L]], decode_field, character(1L))
      terms <- terms[nzchar(terms)]
    }
    specs[[length(specs) + 1L]] <- list(
      label = decode_field(fields[[1L]]),
      response = decode_field(fields[[2L]]),
      terms = terms
    )
  }
  specs
}
is_missing_text <- function(x) is.na(x) | x %in% c("", "NA", "NaN")
convert_values <- function(values, type) {
  values <- as.character(values)
  miss <- is_missing_text(values)
  if (identical(type, "numeric")) {
    x <- suppressWarnings(as.numeric(values))
    x[miss] <- NA_real_
    x
  } else if (identical(type, "logical")) {
    text <- tolower(values)
    x <- rep(NA, length(values))
    x[text %in% c("true", "t", "1", "yes", "y")] <- TRUE
    x[text %in% c("false", "f", "0", "no", "n")] <- FALSE
    x
  } else {
    x <- values
    x[miss] <- NA_character_
    x
  }
}
finalize_types <- function(data, original, completed, types, declared_levels = list()) {
  for (name in names(data)) {
    type <- types[[name]]
    if (type %in% c("factor", "ordered")) {
      all_values <- c(as.character(data[[name]]), as.character(original[[name]]),
                      unlist(lapply(completed, function(one) as.character(one[[name]])), use.names = FALSE))
      levels <- unique(c(as.character(declared_levels[[name]]), all_values[!is.na(all_values)]))
      make_factor <- if (identical(type, "ordered")) ordered else factor
      data[[name]] <- make_factor(as.character(data[[name]]), levels = levels)
      original[[name]] <- make_factor(as.character(original[[name]]), levels = levels)
      completed <- lapply(completed, function(one) {
        one[[name]] <- make_factor(as.character(one[[name]]), levels = levels)
        one
      })
    } else if (identical(type, "character")) {
      data[[name]] <- as.character(data[[name]])
      original[[name]] <- as.character(original[[name]])
      completed <- lapply(completed, function(one) {
        one[[name]] <- as.character(one[[name]])
        one
      })
    }
  }
  list(data = data, original = original, completed = completed)
}
read_payload <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (length(lines) >= 5L && identical(lines[[5L]], "DATACELLS_PERCENT_V1")) {
    lines <- lines[-5L]
    escaped <- which(grepl("%", lines, fixed = TRUE))
    if (length(escaped)) lines[escaped] <- utils::URLdecode(lines[escaped])
  }
  i <- 1L
  next_line <- function() {
    if (i > length(lines)) stop("Malformed native analysis payload.", call. = FALSE)
    value <- lines[[i]]
    i <<- i + 1L
    value
  }
  if (!identical(next_line(), "DATASET")) stop("Malformed native analysis payload.", call. = FALSE)
  group <- next_line()
  rows <- as.integer(next_line())
  cols <- as.integer(next_line())
  data <- list()
  types <- character()
  for (ci in seq_len(cols)) {
    name <- next_line()
    type <- next_line()
    values <- vapply(seq_len(rows), function(ri) next_line(), character(1L))
    data[[name]] <- convert_values(values, type)
    types[[name]] <- type
  }
  data <- as.data.frame(data, check.names = FALSE, stringsAsFactors = FALSE)
  original <- data
  completed <- list(data)
  mask <- as.data.frame(lapply(data, function(x) rep(FALSE, length(x))), stringsAsFactors = FALSE)
  dataset_type <- "data_frame"
  imputation_id <- ""
  source_id <- ""
  imputation_count <- 0L
  active_version <- 1L
  display_mode <- "version"
  declared_levels <- list()
  if (i <= length(lines) && identical(lines[[i]], "DATLEVELS")) {
    i <- i + 1L
    level_columns <- as.integer(next_line())
    for (ci in seq_len(level_columns)) {
      name <- next_line()
      level_count <- as.integer(next_line())
      declared_levels[[name]] <- vapply(
        seq_len(level_count), function(level_index) next_line(), character(1L)
      )
    }
  }
  if (i <= length(lines) && identical(lines[[i]], "IMPUTATION_SPARSE")) {
    i <- i + 1L
    dataset_type <- next_line()
    imputation_id <- next_line()
    source_id <- next_line()
    imputation_count <- as.integer(next_line())
    active_version <- as.integer(next_line())
    display_mode <- next_line()
    mi_cols <- as.integer(next_line())
    completed <- replicate(max(1L, imputation_count), data, simplify = FALSE)
    for (ci in seq_len(mi_cols)) {
      name <- next_line()
      missing_count <- as.integer(next_line())
      missing_rows <- as.integer(vapply(seq_len(missing_count), function(ri) next_line(), character(1L)))
      original_values <- convert_values(vapply(seq_len(missing_count), function(ri) next_line(), character(1L)), types[[name]])
      original[[name]][missing_rows] <- original_values
      mask[[name]][missing_rows] <- TRUE
      for (version in seq_len(imputation_count)) {
        values <- convert_values(vapply(seq_len(missing_count), function(ri) next_line(), character(1L)), types[[name]])
        completed[[version]][[name]][missing_rows] <- values
      }
    }
  }
  typed <- finalize_types(data, original, completed, types, declared_levels)
  data <- typed$data
  original <- typed$original
  completed <- typed$completed
  metadata <- LinkEDA:::.rls_variable_metadata(data)
  for (name in names(types)) {
    row <- metadata$variable_name == name
    metadata$current_analysis_type[row] <- types[[name]]
    metadata$type[row] <- types[[name]]
  }
  list(
    dataset_id = group,
)RLSANALYSIS" R"RLSANALYSIS(name = group,
    dataset_name = group,
    group = group,
    original_data = original,
    data_frame = data,
    data = data,
    n_rows = nrow(data),
    n_columns = ncol(data),
    original_row_ids = seq_len(nrow(data)),
    selection_state = integer(0),
    row_color_state = character(0),
    metadata = metadata,
    variable_metadata = metadata,
    dataset_type = dataset_type,
    imputation_id = imputation_id,
    source_dataset_id = source_id,
    completed_datasets = completed,
    missing_cell_mask = mask,
    active_imputation_version = active_version,
    imputation_display_mode = display_mode,
    imputation_count = imputation_count,
    source = "Native R/mice analysis bridge",
    row_colors = character(0),
    modified = FALSE
  )
}
register_record <- function(record) {
  LinkEDA:::.rls_set_dataset_record(record)
  state <- get(".rls_state", envir = asNamespace("LinkEDA"))
  state$active_dataset <- record$group
  invisible(record)
}

tryCatch({
  record <- register_record(read_payload(input))
  variables <- split_names(variables_text)
  emm_test_reference <- NULL
  plot_focal <- NULL
  report_focal <- NULL
  report_group <- NULL
  effect_quantity <- "predicted_probability"
  effect_adjustment <- "average_sample"
  effect_presentation <- "percentage"
  effect_confidence_level <- .95
  emm_reference_option <- variables[startsWith(variables, "emm_reference=")]
  if (length(emm_reference_option)) {
    reference_text <- sub("^emm_reference=", "", tail(emm_reference_option, 1L))
    if (!identical(reference_text, "none")) {
      emm_test_reference <- suppressWarnings(as.numeric(reference_text))
      if (length(emm_test_reference) != 1L || !is.finite(emm_test_reference)) {
        stop("The estimated-mean reference value must be a finite number.", call. = FALSE)
      }
    }
  }
  plot_focal_option <- variables[startsWith(variables, "plot_focal=")]
  if (length(plot_focal_option)) {
    candidate <- sub("^plot_focal=", "", tail(plot_focal_option, 1L))
    if (nzchar(candidate)) plot_focal <- candidate
  }
  report_focal_option <- variables[startsWith(variables, "report_focal=")]
  if (length(report_focal_option)) {
    candidate <- sub("^report_focal=", "", tail(report_focal_option, 1L))
    if (nzchar(candidate)) report_focal <- candidate
  }
  report_group_option <- variables[startsWith(variables, "report_group=")]
  if (length(report_group_option)) {
    candidate <- sub("^report_group=", "", tail(report_group_option, 1L))
    if (nzchar(candidate)) report_group <- candidate
  }
  effect_quantity_option <- variables[startsWith(variables, "effect_quantity=")]
  if (length(effect_quantity_option)) {
    effect_quantity <- sub("^effect_quantity=", "", tail(effect_quantity_option, 1L))
  }
  effect_adjustment_option <- variables[startsWith(variables, "effect_adjustment=")]
  if (length(effect_adjustment_option)) {
    effect_adjustment <- sub("^effect_adjustment=", "", tail(effect_adjustment_option, 1L))
  }
  effect_presentation_option <- variables[startsWith(variables, "effect_presentation=")]
  if (length(effect_presentation_option)) {
    effect_presentation <- sub("^effect_presentation=", "", tail(effect_presentation_option, 1L))
  }
  effect_confidence_option <- variables[startsWith(variables, "effect_confidence=")]
  if (length(effect_confidence_option)) {
    effect_confidence_level <- suppressWarnings(as.numeric(sub(
      "^effect_confidence=", "", tail(effect_confidence_option, 1L)
    )))
    if (length(effect_confidence_level) != 1L || !is.finite(effect_confidence_level) ||
        effect_confidence_level <= 0 || effect_confidence_level >= 1) {
      stop("The effect confidence level must be between zero and one.", call. = FALSE)
    }
  }
  terms <- split_names(terms_text)
  group_var <- scalar_text(group_text)
  response <- scalar_text(response_text)
  scope <- scalar_text(scope_text)
  if (!nzchar(scope)) scope <- "all"

  if (identical(mode, "contingency")) {
    spec <- strsplit(model_spec_text, "|", fixed = TRUE)[[1L]]
    version <- suppressWarnings(as.integer(spec[[1L]]))
    if (!is.finite(version) || version < 1L) stop("Invalid contingency data version.")
    record$data_version <- version
    register_record(record)
    selected <- if (identical(scope, "selected")) {
      text <- if (length(spec) >= 2L) spec[[2L]] else ""
      as.integer(split_names(text))
    } else NULL
    table_record <- LinkEDA:::.rls_mi_contingency_record(
      record$group, variables, group_var, id = analysis_id, selected_rows = selected,
      display_mode = if(length(spec)>=3L && nzchar(spec[[3L]])) spec[[3L]] else "count_percent")
    writeLines(LinkEDA:::.rls_table1_native_payload(table_record), output, useBytes = TRUE)
  } else if (identical(mode, "table1")) {
    table1_selected_rows <- if (identical(scope, "selected")) {
      rows <- suppressWarnings(as.integer(split_names(model_spec_text)))
      rows[is.finite(rows) & rows > 0L]
    } else {
      NULL
    }
    handle <- LinkEDA::ls_new_table1(
      record$group,
      variables = if (length(variables)) variables else NULL,
      group = if (nzchar(group_var)) group_var else NULL,
      name = if (nzchar(analysis_id)) analysis_id else NULL,
      .selected_rows = table1_selected_rows,
      .scope_description = if (identical(scope, "selected")) "Selected rows" else NULL
    )
    table_record <- LinkEDA:::.rls_table1_record(handle)
    writeLines(LinkEDA:::.rls_table1_native_payload(table_record), output, useBytes = TRUE)
  } else if (mode %in% c("glm", "glm_pairwise", "glm_interaction", "glm_interaction_plot", "glm_partial_plot")) {
    if (!length(terms)) stop("Add at least one independent variable before opening the pooled MI General Linear Model table.", call. = FALSE)
    model <- LinkEDA::ls_new_glm(record$group)
    if (nzchar(response)) model <- LinkEDA::ls_glm_set_dependent(model, response)
    for (term in terms) model <- LinkEDA::ls_glm_add_predictor(model, term)
    model_record <- LinkEDA:::.rls_glm_model_record(model)
    # The native table has already fitted this specification with the selected
    # scope.  Post-estimation runs in a separate R process, so it must restore
    # that scope explicitly as well as the encoded row ids.  Otherwise the
    # interaction report/plot silently refits all rows and resurrects factor
    # levels that were absent from the fitted analysis sample.
    model_record$scope <- scope
    if (nzchar(model_spec_text)) {
      fields <- strsplit(model_spec_text, "|", fixed = TRUE)[[1L]]
      if (identical(fields[[1L]], "MI_LINEAR_SPEC_V1")) {
        cursor <- 2L
        take <- function() {
          if (cursor > length(fields)) stop("The standalone MI linear-model specification is incomplete.", call. = FALSE)
          value <- decode_field(fields[[cursor]])
          cursor <<- cursor + 1L
          value
        }
        take_count <- function(label) {
          value <- suppressWarnings(as.integer(take()))
          if (!is.finite(value) || value < 0L) stop(sprintf("The standalone MI %s count is invalid.", label), call. = FALSE)
          value
        }
        type_count <- take_count("term-type")
        if (type_count > 0L) for (i in seq_len(type_count)) {
          term_name <- take()
          model_record$term_types[[term_name]] <- take()
        }
        centered_count <- take_count("centered-predictor")
        if (centered_count > 0L) model_record$centered_predictors <- vapply(seq_len(centered_count), function(i) take(), character(1L))
        reference_count <- take_count("factor-reference")
        if (reference_count > 0L) for (i in seq_len(reference_count)) {
          term_name <- take()
          model_record$factor_reference_levels[[term_name]] <- take()
        }
        row_count <- take_count("selected-row")
        if (row_count > 0L) model_record$selected_rows <- suppressWarnings(as.integer(vapply(seq_len(row_count), function(i) take(), character(1L))))
      } else {
        centered <- split_names(model_spec_text)
        model_record$centered_predictors <- unique(centered[nzchar(centered)])
      }
    }
    model <- LinkEDA:::.rls_assign_glm_model(model_record)
    model <- LinkEDA::ls_glm_fit(model)
    model_record <- LinkEDA:::.rls_glm_model_record(model)
    if (identical(mode, "glm_pairwise")) {
      if (!nzchar(group_var)) stop("Choose a categorical term for pairwise comparisons.", call. = FALSE)
      writeLines(LinkEDA:::.rls_pairwise_native_payload(
        LinkEDA::ls_glm_pairwise(model, group_var)
      ), output, useBytes = TRUE)
    } else if (identical(mode, "glm_partial_plot")) {
      if (!nzchar(group_var)) stop("Choose a fitted term for the partial regression plot.", call. = FALSE)
      residual_type <- if (length(variables)) variables[[1L]] else "default"
      writeLines(LinkEDA:::.rls_partial_native_plot_payload(
        LinkEDA:::.rls_regression_partial_plot_record(model_record, group_var, residual_type)
      ), output, useBytes = TRUE)
    } else if (mode %in% c("glm_interaction", "glm_interaction_plot")) {
      if (!nzchar(group_var)) stop("Choose a two- or three-way interaction to interpret.", call. = FALSE)
      interaction <- LinkEDA::ls_glm_interaction(
        model, group_var, confidence_level = effect_confidence_level,
        emm_test_reference = emm_test_reference,
        focal = if (identical(mode, "glm_interaction_plot")) plot_focal else report_focal,
        group_by = if (identical(mode, "glm_interaction_plot")) NULL else report_group,
        quantity = effect_quantity, adjustment = effect_adjustment,
        presentation = effect_presentation
      )
      writeLines(if (identical(mode, "glm_interaction_plot")) {
        LinkEDA:::.rls_interaction_native_plot_payload(interaction)
)RLSANALYSIS" R"RLSANALYSIS(      } else {
        LinkEDA:::.rls_interaction_native_report(interaction)
      }, output, useBytes = TRUE)
    } else {
      writeLines(LinkEDA:::.rls_glm_pooled_native_payload(model_record), output, useBytes = TRUE)
    }
  } else if (mode %in% c("gglm", "gglm_pairwise", "gglm_interaction", "gglm_interaction_plot", "gglm_partial_plot")) {
    if (!nzchar(response)) stop("Choose a response variable before opening the pooled MI Generalized Linear Model table.", call. = FALSE)
    family <- "gaussian"
    link <- ""
    generation <- 0L
    analysis_mode <- "GENERALIZED"
    model_type <- "legacy_generalized"
    count_distribution <- "poisson"
    exposure <- ""
    offset <- ""
    trials_variable <- ""
    trials_constant <- NA_real_
    event <- ""
    reference <- ""
    response_bounds <- NULL
    term_types <- list()
    centered_predictors <- character()
    factor_reference_levels <- list()
    selected_rows <- integer()
    if (nzchar(model_spec_text)) {
      fields <- strsplit(model_spec_text, "|", fixed = TRUE)[[1L]]
      if (fields[[1L]] %in% c("MI_GGLM_SPEC_V2", "MI_GGLM_SPEC_V3", "MI_GGLM_SPEC_V4", "MI_GGLM_SPEC_V5")) {
        bounded_spec <- identical(fields[[1L]], "MI_GGLM_SPEC_V3")
        current_spec <- fields[[1L]] %in% c("MI_GGLM_SPEC_V4", "MI_GGLM_SPEC_V5")
        cursor <- 2L
        take <- function() {
          if (cursor > length(fields)) stop("The standalone MI generalized-model specification is incomplete.", call. = FALSE)
          value <- decode_field(fields[[cursor]])
          cursor <<- cursor + 1L
          value
        }
        take_count <- function(label) {
          value <- suppressWarnings(as.integer(take()))
          if (!is.finite(value) || value < 0L) {
            stop(sprintf("The standalone MI %s count is invalid.", label), call. = FALSE)
          }
          value
        }
        family <- take()
        link <- take()
        generation <- suppressWarnings(as.integer(take()))
        if (!is.finite(generation) || generation < 0L) generation <- 0L
        analysis_mode <- take()
        count_distribution <- take()
        exposure <- take()
        if (identical(fields[[1L]], "MI_GGLM_SPEC_V5")) offset <- take()
        if (current_spec) {
          trials_variable <- take()
          trials_constant <- suppressWarnings(as.numeric(take()))
        }
        event <- take()
        reference <- take()
        if (current_spec) {
          bounds_configured <- identical(take(), "TRUE")
          lower <- suppressWarnings(as.numeric(take()))
          upper <- suppressWarnings(as.numeric(take()))
          if (bounds_configured) {
            if (!is.finite(lower) || !is.finite(upper) || lower >= upper) {
              stop("The standalone MI generalized-model response bounds are invalid.", call. = FALSE)
            }
            response_bounds <- c(lower, upper)
          }
        } else if (bounded_spec) {
          lower <- suppressWarnings(as.numeric(take()))
          upper <- suppressWarnings(as.numeric(take()))
          if (!is.finite(lower) || !is.finite(upper) || lower >= upper) {
            stop("The standalone MI generalized-model response bounds are invalid.", call. = FALSE)
          }
          response_bounds <- c(lower, upper)
        }
        type_count <- take_count("term-type")
        if (type_count > 0L) for (i in seq_len(type_count)) {
          term_name <- take()
          term_types[[term_name]] <- take()
        }
        centered_count <- take_count("centered-predictor")
        if (centered_count > 0L) centered_predictors <- vapply(seq_len(centered_count), function(i) take(), character(1L))
        reference_count <- take_count("factor-reference")
        if (reference_count > 0L) for (i in seq_len(reference_count)) {
          term_name <- take()
          factor_reference_levels[[term_name]] <- take()
        }
        row_count <- take_count("selected-row")
        if (row_count > 0L) {
          selected_rows <- suppressWarnings(as.integer(vapply(seq_len(row_count), function(i) take(), character(1L))))
          if (any(!is.finite(selected_rows)) || any(selected_rows < 1L)) {
            stop("The standalone MI selected rows are invalid.", call. = FALSE)
          }
        }
        if (cursor <= length(fields) && identical(take(), "MODEL_TYPE_V1") &&
            cursor <= length(fields)) {
          model_type <- take()
        }
      } else {
        if (length(fields) >= 1L && nzchar(fields[[1L]])) family <- decode_field(fields[[1L]])
        if (length(fields) >= 2L && nzchar(fields[[2L]])) link <- decode_field(fields[[2L]])
        if (length(fields) >= 3L && nzchar(fields[[3L]])) {
          generation <- suppressWarnings(as.integer(fields[[3L]]))
          if (!is.finite(generation) || generation < 0L) generation <- 0L
        }
      }
    }
    if (!length(terms) && !analysis_mode %in% c("COUNT", "BINARY")) {
      stop("Add at least one predictor before opening the pooled MI Generalized Linear Model table.", call. = FALSE)
    }
    common <- list(
      data = record$group, response = response, terms = terms, scope = scope,
      name = if (nzchar(analysis_id)) analysis_id else NULL, native = FALSE,
      term_types = term_types, centered_predictors = centered_predictors,
      factor_reference_levels = factor_reference_levels,
      offset = if (nzchar(offset)) offset else NULL,
      .selected_rows = selected_rows
    )
    model <- if (identical(analysis_mode, "COUNT")) {
      do.call(LinkEDA::ls_new_count_regression, c(common, list(
        distribution = count_distribution,
        exposure = if (nzchar(exposure)) exposure else NULL,
        trials = if (nzchar(trials_variable)) trials_variable else trials_constant,
        .allow_intercept_only = TRUE
      )))
    } else if (identical(analysis_mode, "BINARY")) {
      do.call(LinkEDA::ls_new_binary_regression, c(common, list(
        link = link,
        event = if (nzchar(event)) event else NULL,
        reference = if (nzchar(reference)) reference else NULL
      )))
    } else {
      do.call(LinkEDA::ls_new_generalized_linear_model, c(common, list(
        family = family, link = if (nzchar(link)) link else NULL,
        response_bounds = response_bounds, .model_type = model_type,
        .allow_intercept_only = TRUE
      )))
    }
    model_record <- LinkEDA:::.rls_generalized_glm_record(model)
    model_record$native_generation <- generation
    if (isTRUE(model_record$count_regression) && !length(effect_quantity_option)) {
      if (identical(model_record$count_distribution,
                    "hurdle_beta_binomial_ceiling")) {
        effect_quantity <- "overall_expected_score"
      } else if (model_record$count_distribution %in%
                 c("binomial_trials", "beta_binomial")) {
        effect_quantity <- "expected_count"
      } else if (identical(model_record$count_distribution, "perfect_score")) {
        effect_quantity <- "predicted_probability"
      }
    }
    if (identical(mode, "gglm_partial_plot")) {
      if (!nzchar(group_var)) stop("Choose a fitted term for the partial regression plot.", call. = FALSE)
      residual_type <- if (length(variables)) variables[[1L]] else "default"
      writeLines(LinkEDA:::.rls_partial_native_plot_payload(
        LinkEDA:::.rls_regression_partial_plot_record(model_record, group_var, residual_type)
      ), output, useBytes = TRUE)
    } else if (identical(mode, "gglm_pairwise")) {
      if (!nzchar(group_var)) stop("Choose a categorical term for pairwise comparisons.", call. = FALSE)
      result <- if (identical(analysis_mode, "BINARY")) {
        LinkEDA::ls_binary_regression_pairwise(model, group_var)
      } else {
        LinkEDA::ls_generalized_linear_model_pairwise(model, group_var, scale = "response")
      }
      writeLines(LinkEDA:::.rls_pairwise_native_payload(result), output, useBytes = TRUE)
    } else if (mode %in% c("gglm_interaction", "gglm_interaction_plot")) {
      if (!nzchar(group_var)) stop("Choose a two- or three-way interaction to interpret.", call. = FALSE)
      interaction <- LinkEDA::ls_generalized_linear_model_interaction(
        model, group_var, confidence_level = effect_confidence_level,
        emm_test_reference = emm_test_reference,
        focal = if (identical(mode, "gglm_interaction_plot")) plot_focal else report_focal,
        group_by = if (identical(mode, "gglm_interaction_plot")) NULL else report_group,
        quantity = effect_quantity, adjustment = effect_adjustment,
        presentation = effect_presentation
      )
      writeLines(if (identical(mode, "gglm_interaction_plot")) {
        LinkEDA:::.rls_interaction_native_plot_payload(interaction)
      } else {
        LinkEDA:::.rls_interaction_native_report(interaction)
      }, output, useBytes = TRUE)
    } else {
      writeLines(LinkEDA:::.rls_generalized_glm_pooled_native_payload(model_record), output, useBytes = TRUE)
    }
  } else if (identical(mode, "regcmp")) {
    model_specs <- parse_model_specs(model_spec_text)
    if (length(model_specs)) {
      model_responses <- vapply(model_specs, function(spec) spec$response, character(1L))
      shared_response <- if (nzchar(response) && all(model_responses == response)) response else NULL
      models <- lapply(model_specs, function(spec) {
        if (is.null(shared_response)) {
          list(response = spec$response, terms = spec$terms)
        } else {
          spec$terms
        }
      })
      labels <- vapply(seq_along(model_specs), function(i) {
        label <- model_specs[[i]]$label
        if (nzchar(label)) label else sprintf("Model %d", i)
      }, character(1L))
      names(models) <- labels
      response_arg <- shared_response
    } else {
      if (!nzchar(response)) stop("Choose a response variable before opening pooled MI model comparison.", call. = FALSE)
      models <- list(terms)
      names(models) <- "Model 1"
      response_arg <- response
    }
    comparison <- LinkEDA::ls_new_regression_comparison(
      record$group,
      response = response_arg,
      models = models,
      scope = scope,
      name = if (nzchar(analysis_id)) analysis_id else NULL,
      native = FALSE
    )
    comparison_record <- LinkEDA:::.rls_regcmp_record(comparison)
    writeLines(LinkEDA:::.rls_regcmp_pooled_native_payload(comparison_record), output, useBytes = TRUE)
  } else {
    stop(sprintf("Unknown native R/mice analysis mode: %s", mode), call. = FALSE)
  }
}, error = function(e) {
  message(conditionMessage(e))
  quit(status = 1L)
})
)RLSANALYSIS";
}

std::string NormalizeVariableType(const std::string &type)
{
    std::string lowered = LowerCopy(TrimCopy(type));
    if (lowered == "numeric" || lowered == "number" ||
        lowered == "numerico" || lowered == "numérico") {
        return "numeric";
    }
    if (lowered == "factor" || lowered == "categorical") {
        return "factor";
    }
    if (lowered == "ordered" || lowered == "ordered_factor" ||
        lowered == "ordered factor" || lowered == "factor ordenado" ||
        lowered == "ordinal") {
        return "ordered";
    }
    if (lowered == "character" || lowered == "text" ||
        lowered == "texto" || lowered == "string") {
        return "character";
    }
    if (lowered == "logical" || lowered == "boolean" || lowered == "bool" ||
        lowered == "binary") {
        return "logical";
    }
    return lowered;
}

bool VariableTypeIsSupported(const std::string &type)
{
    std::string normalized = NormalizeVariableType(type);
    return normalized == "numeric" || normalized == "factor" ||
        normalized == "ordered" || normalized == "character" ||
        normalized == "logical";
}

bool VariableTypeIsNumeric(const std::string &type)
{
    return NormalizeVariableType(type) == "numeric";
}

bool VariableTypeIsCategorical(const std::string &type)
{
    const std::string normalized = NormalizeVariableType(type);
    return normalized == "factor" || normalized == "ordered" ||
        normalized == "logical";
}

bool VariableTypeIsOrdinal(const std::string &type)
{
    return NormalizeVariableType(type) == "ordered";
}

bool VariableTypeIsText(const std::string &type)
{
    return NormalizeVariableType(type) == "character";
}

bool VariableTypeIsFactorLike(const std::string &type)
{
    return VariableTypeIsCategorical(type);
}

std::string VariableTypeDisplayName(const std::string &type)
{
    std::string normalized = NormalizeVariableType(type);
    if (normalized == "numeric") return "Numeric";
    if (normalized == "factor") return "Categorical";
    if (normalized == "ordered") return "Ordinal";
    if (normalized == "character") return "Text";
    if (normalized == "logical") return "Categorical (binary)";
    return normalized.empty() ? "unknown" : normalized;
}

std::string VariableRoleDisplayName(const std::string &role)
{
    if (role == "Y" || role == "dependent") return "Dependent";
    if (role == "Predictor" || role == "independent" || role == "predictor") return "Independent";
    return "None";
}

std::string VariableTypeEditingStatus(const std::string &variable)
{
    return "Editing type for `" + variable + "`.";
}

std::string VariableTypeChangedStatus(const std::string &variable,
                                      const std::string &type)
{
    return variable + " is now treated as " + VariableTypeDisplayName(type) + ".";
}

std::string VariableDecimalsUnavailableStatus(const std::string &variable,
                                              const std::string &type)
{
    return "Decimals are available only for Numeric variables; `" + variable +
        "` is " + VariableTypeDisplayName(type) + ".";
}

std::string VariableDecimalsInvalidStatus(bool bounded)
{
    return bounded
        ? "Decimals must be an integer from 0 to 12, or Automatic."
        : "Decimals must be an integer or blank for automatic.";
}

std::string VariableDecimalsChangedStatus(const std::string &variable,
                                          int decimals)
{
    return decimals < 0
        ? "Decimals for `" + variable + "` set to automatic."
        : "Decimals for `" + variable + "` set to " + std::to_string(decimals) + ".";
}

std::string VariableDescriptionChangedStatus(const std::string &variable)
{
    return "Updated description for `" + variable + "`.";
}

std::string VariableRenamedStatus(const std::string &oldName,
                                  const std::string &newName)
{
    return "Renamed `" + oldName + "` to `" + newName + "`.";
}

std::string VariableNotFoundStatus(const std::string &variable)
{
    return "Variable `" + variable + "` was not found.";
}

std::string VariableNotFoundInDatasetStatus(const std::string &variable,
                                            const std::string &group)
{
    return "Variable `" + variable + "` was not found in dataset `" + group + "`.";
}

std::string VariableUnavailableStatus(const std::string &variable)
{
    return "Variable `" + variable + "` is not available.";
}

std::string DerivedDataColumnAddedStatus(const std::string &column,
                                         const std::string &group)
{
    return "Added `" + column + "` to dataset `" + group + "`.";
}

std::string VariableRoleChangedStatus(const std::string &variable,
                                      const std::string &roleAction)
{
    if (roleAction == "dependent") {
        return "Set the default role for `" + variable + "` to Dependent.";
    }
    if (roleAction == "predictor" || roleAction == "independent") {
        return "Set the default role for `" + variable + "` to Independent.";
    }
    if (roleAction == "remove_predictor") {
        return "Cleared the default role for `" + variable + "`.";
    }
    if (roleAction == "none" || roleAction == "clear") {
        return "Cleared the default role for `" + variable + "`.";
    }
    return "Updated the default role for `" + variable + "`.";
}

std::string VariableViewModelRoleSummary(const std::string &dependent,
                                         std::size_t predictorCount)
{
    return "Default roles: dependent=" +
        (dependent.empty() ? std::string("(none)") : dependent) +
        " | independent=" + std::to_string(predictorCount);
}

bool DataColumnAllowsNumeric(const DataColumn &col, std::string *message)
{
    double parsed = NAN;
    for (const std::string &value : col.values) {
        if (DataCellIsMissing(value)) {
            continue;
        }
        if (!ParseDataCellDouble(value, parsed)) {
            if (message) {
                *message = "Variable `" + col.name +
                    "` cannot be treated as numeric because some values cannot be converted.";
            }
            return false;
        }
    }
    return true;
}

bool DataColumnHasMissing(const DataColumn &col)
{
    for (const std::string &value : col.values) {
        if (DataCellIsMissing(value)) {
            return true;
        }
    }
    return false;
}

bool DataColumnAllMissing(const DataColumn &col)
{
    if (col.values.empty()) {
        return true;
    }
    for (const std::string &value : col.values) {
        if (!DataCellIsMissing(value)) {
            return false;
        }
    }
    return true;
}

bool DataColumnSupportedForMice(const DataColumn &col)
{
    std::string type = NormalizeVariableType(col.type);
    return type == "numeric" || type == "factor" || type == "ordered" ||
        type == "character" || type == "logical";
}

bool DataColumnLooksLikeId(const DataColumn &col)
{
    std::string name = LowerCopy(col.name);
    if (name == "id" || name.find("_id") != std::string::npos ||
        name.find("identifier") != std::string::npos ||
        name.find("subject") != std::string::npos ||
        name.find("case") != std::string::npos ||
        name.find("etiqueta") != std::string::npos ||
        name.find("label") != std::string::npos) {
        return true;
    }

    std::vector<std::string> observed;
    observed.reserve(col.values.size());
    for (const std::string &value : col.values) {
        if (!DataCellIsMissing(value)) {
            observed.push_back(value);
        }
    }
    if (observed.empty()) {
        return false;
    }
    std::set<std::string> uniqueValues(observed.begin(), observed.end());
    return uniqueValues.size() == observed.size() &&
        observed.size() >= static_cast<std::size_t>(
        std::ceil(0.9 * static_cast<double>(std::max<std::size_t>(1, col.values.size()))));
}

bool DataColumnLooksLikeAnalysisId(const DataColumn &col)
{
    std::string name = LowerCopy(col.name);
    if (name == "id" || name.find("_id") != std::string::npos ||
        name.find("identifier") != std::string::npos ||
        name.find("subject") != std::string::npos ||
        name.find("case") != std::string::npos ||
        name.find("etiqueta") != std::string::npos ||
        name.find("label") != std::string::npos) {
        return true;
    }
    if (!VariableTypeIsNumeric(col.type)) return DataColumnLooksLikeId(col);

    // A continuous measurement can legitimately contain one distinct value
    // per case. Exclude it from response selectors only when it is the usual
    // implicit row identifier: a unique integer sequence such as 1..N.
    std::vector<double> observed;
    observed.reserve(col.values.size());
    for (auto const& value : col.values) {
        if (DataCellIsMissing(value)) continue;
        double parsed = NAN;
        if (!ParseDataCellDouble(value, parsed) || !std::isfinite(parsed) ||
            std::fabs(parsed - std::round(parsed)) > 1.0e-9) return false;
        observed.push_back(parsed);
    }
    if (observed.empty()) return false;
    std::sort(observed.begin(), observed.end());
    if (std::adjacent_find(observed.begin(), observed.end()) != observed.end())
        return false;
    for (std::size_t index = 1; index < observed.size(); ++index)
        if (std::fabs((observed[index] - observed[index - 1]) - 1.0) > 1.0e-9)
            return false;
    return true;
}

bool DataColumnLooksBinaryNumeric(const DataColumn &col)
{
    std::set<std::string> levels;
    for (const std::string &value : col.values) {
        if (DataCellIsMissing(value)) continue;
        double numeric = NAN;
        if (!ParseDataCellDouble(value, numeric)) return false;
        if (std::fabs(numeric) > 1.0e-9 && std::fabs(numeric - 1.0) > 1.0e-9) return false;
        levels.insert(std::fabs(numeric - 1.0) <= 1.0e-9 ? "1" : "0");
    }
    return levels.size() == 2;
}

bool DataColumnIsBinaryCategorical(const DataColumn &col)
{
    if (!VariableTypeIsCategorical(col.type)) return false;
    return DataColumnFactorLevels(col).size() == 2;
}

std::string DataColumnStorageType(const DataColumn &col)
{
    if (!col.storageType.empty()) return col.storageType;
    const std::string type = NormalizeVariableType(col.type);
    if (type == "numeric") return "double";
    if (type == "ordered") return "ordered factor";
    if (type == "factor") return "factor";
    if (type == "logical") return "logical";
    if (type == "character") return "character";
    return type.empty() ? "unknown" : type;
}

bool DataColumnLooksGroupingCandidate(const DataColumn &col, int rows)
{
    if (DataColumnAllMissing(col) || DataColumnLooksLikeId(col)) return false;
    int levels = DataColumnObservedLevelCount(col);
    if (levels < 2) return false;
    if (NormalizeVariableType(col.type) == "numeric") {
        return levels <= std::max(20, static_cast<int>(std::ceil(static_cast<double>(std::max(1, rows)) / 4.0)));
    }
    return true;
}

int DataColumnMissingCount(const DataColumn &col)
{
    int count = 0;
    for (const std::string &value : col.values) {
        if (DataCellIsMissing(value)) {
            ++count;
        }
    }
    return count;
}

int DataColumnObservedLevelCount(const DataColumn &col)
{
    return static_cast<int>(DataColumnObservedLevels(col).size());
}

std::vector<std::string> DataColumnObservedLevels(const DataColumn &col)
{
    std::vector<std::string> levels;
    for (std::size_t row = 0; row < col.values.size(); ++row) {
        const std::string value = UnformattedDisplayValue(col, row);
        if (!DataCellIsMissing(value) &&
            std::find(levels.begin(), levels.end(), value) == levels.end()) {
            levels.push_back(value);
        }
    }
    return levels;
}

std::vector<std::string> DataColumnFactorLevels(const DataColumn &col)
{
    std::vector<std::string> levels;
    auto append = [&](const std::string &value) {
        if (!DataCellIsMissing(value) &&
            std::find(levels.begin(), levels.end(), value) == levels.end()) {
            levels.push_back(value);
        }
    };
    for (const std::string &value : col.definedLevels) append(value);
    for (const std::string &value : DataColumnObservedLevels(col)) append(value);
    return levels;
}

std::string DefaultMiceMethod(const DataColumn &col)
{
    std::string type = NormalizeVariableType(col.type);
    if (type == "numeric") return "pmm";
    if (type == "logical") return "logreg";
    if (type == "ordered") {
        return DataColumnObservedLevelCount(col) <= 2 ? "logreg" : "polr";
    }
    if (type == "factor" || type == "character") {
        return DataColumnObservedLevelCount(col) <= 2 ? "logreg" : "polyreg";
    }
    return "";
}

std::vector<std::string> MiceMethodOptions(const DataColumn &col)
{
    std::string type = NormalizeVariableType(col.type);
    if (type == "numeric") return {"pmm", "norm", "cart"};
    if (type == "logical") return {"logreg"};
    if (type == "ordered") {
        if (DataColumnObservedLevelCount(col) <= 2) return {"logreg"};
        return {"polr", "cart"};
    }
    if (type == "factor" || type == "character") {
        if (DataColumnObservedLevelCount(col) <= 2) return {"logreg"};
        return {"polyreg", "cart"};
    }
    return {};
}

std::string MiceVariableStatusText(const DataColumn &col, int rowCount)
{
    const std::string normalized = NormalizeVariableType(col.type);
    std::string status = col.type.empty() ? "unknown" :
        (VariableTypeIsSupported(normalized) ? VariableTypeDisplayName(normalized) : col.type);
    int missing = DataColumnMissingCount(col);
    status += ", " + std::to_string(missing) + " missing";
    if (missing >= std::max(1, rowCount)) status += ", all missing";
    if (DataColumnLooksLikeId(col)) status += ", id-like";
    if (!DataColumnSupportedForMice(col)) {
        status += ", " + BuildMissingDataImputationDialogState().unsupportedMethodTitle;
    }
    return status;
}

std::vector<std::string> DefaultImputeVariables(const DataFrameModel &df)
{
    std::vector<std::string> out;
    for (const DataColumn &col : df.columns) {
        if (DataColumnSupportedForMice(col) && DataColumnHasMissing(col) &&
            !DataColumnAllMissing(col) && !DataColumnLooksLikeId(col)) {
            out.push_back(col.name);
        }
    }
    return out;
}

std::vector<std::string> DefaultPredictorVariables(const DataFrameModel &df)
{
    std::vector<std::string> out;
    for (const DataColumn &col : df.columns) {
        if (DataColumnSupportedForMice(col) && !DataColumnAllMissing(col) &&
            !DataColumnLooksLikeId(col)) {
            out.push_back(col.name);
        }
    }
    return out;
}

std::string MiceDatasetSummaryText(const DataFrameModel &df,
                                   int missingColumnCount,
                                   int imputeColumnCount)
{
    std::ostringstream summary;
    summary << df.group << ": " << df.rows << " rows, "
            << df.columns.size() << " variables, "
            << missingColumnCount << " variables with missing values, "
            << imputeColumnCount << " selected for imputation.";
    return summary.str();
}

std::string MiceSelectionSummaryText(std::size_t imputeCount,
                                     std::size_t predictorCount)
{
    return std::to_string(imputeCount) + " variables to impute, " +
        std::to_string(predictorCount) + " predictors selected.";
}

bool SetDataColumnType(DataColumn &col, const std::string &type, std::string *message,
                       const VariableTypeConversionSpecification *conversion)
{
    if (message) message->clear();
    std::string normalized = NormalizeVariableType(type);
    if (!VariableTypeIsSupported(normalized)) {
        if (message) {
            *message = "Variable type must be Numeric, Categorical, Ordinal, or Text.";
        }
        return false;
    }
    const std::string previousType = NormalizeVariableType(col.type);
    if (normalized == "numeric" && !ConvertColumnToNumeric(col, message, conversion)) return false;
    if ((normalized == "factor" || normalized == "ordered") && conversion &&
        !conversion->categoryOrder.empty()) {
        const std::vector<std::string> existing = ObservedDisplayLevels(col);
        const std::set<std::string> provided(conversion->categoryOrder.begin(),
                                             conversion->categoryOrder.end());
        const std::set<std::string> expected(existing.begin(), existing.end());
        if (provided != expected || provided.size() != conversion->categoryOrder.size()) {
            if (message) *message = "Category order must contain every category exactly once.";
            return false;
        }
        col.definedLevels = conversion->categoryOrder;
    }
    if ((normalized == "factor" || normalized == "ordered") &&
        !EncodeCategoricalColumn(col, message)) return false;
    if (normalized == "character" && !col.displayValues.empty()) {
        std::vector<std::string> labels;
        labels.reserve(col.values.size());
        for (std::size_t row = 0; row < col.values.size(); ++row)
            labels.push_back(UnformattedDisplayValue(col, row));
        col.values = std::move(labels);
        col.displayValues.clear();
        col.definedLevels.clear();
    }
    if (normalized == "character") {
        col.reversibleFactorLevels.clear();
        col.numericMapping.clear();
        col.reversibleCategoryType.clear();
    }
    col.type = normalized;
    col.storageType = normalized == "numeric" ? "double" :
        normalized == "ordered" ? "ordered factor" :
        normalized == "factor" ? "factor" :
        normalized == "logical" ? "logical" : "character";
    if (previousType == "logical" && normalized == "factor" && col.definedLevels.empty())
        col.definedLevels = {"FALSE", "TRUE"};
    col.binary = DataColumnIsBinaryCategorical(col);
    return true;
}

bool SetDataColumnDescription(DataColumn &col, const std::string &description)
{
    col.description = description;
    return true;
}

bool SetDataFrameCellValue(DataFrameModel &df,
                           const std::string &variable,
                           std::size_t row,
                           const std::string &value,
                           std::string *message)
{
    DataColumn *column = FindDataColumnInDataFrame(df, variable);
    if (!column) {
        if (message) *message = "Variable `" + variable + "` was not found.";
        return false;
    }
    if (row >= static_cast<std::size_t>(std::max(0, df.rows))) {
        if (message) *message = "The edited row is outside the dataset.";
        return false;
    }
    const bool imputedCell = DataFrameCellIsImputed(df, *column, row);
    if (imputedCell && df.imputationDisplayMode != "version") {
        if (message) *message =
            "Choose one imputation version before editing an imputed cell.";
        return false;
    }
    std::string stored = TrimCopy(value);
    const bool missing = stored.empty() || stored == "NA" || stored == "NaN";
    const std::string type = NormalizeVariableType(column->type);
    if (!missing && type == "numeric") {
        double parsed = NAN;
        if (!ParseDataCellDouble(stored, parsed)) {
            if (message) *message = "`" + value + "` is not a valid numeric value.";
            return false;
        }
        std::ostringstream normalized;
        normalized << std::setprecision(17) << parsed;
        stored = normalized.str();
    } else if (!missing && type == "logical") {
        const std::string lowered = LowerCopy(stored);
        if (lowered == "true" || lowered == "t" || lowered == "yes" || lowered == "y" || lowered == "1") {
            stored = "TRUE";
        } else if (lowered == "false" || lowered == "f" || lowered == "no" || lowered == "n" || lowered == "0") {
            stored = "FALSE";
        } else {
            if (message) *message = "Logical values must be TRUE, FALSE, 1, 0, yes, or no.";
            return false;
        }
    }
    if (missing) stored = "NA";
    if (column->values.size() < static_cast<std::size_t>(df.rows)) {
        column->values.resize(static_cast<std::size_t>(df.rows), "NA");
    }
    const bool encodedFactor = (type == "factor" || type == "ordered") &&
        !column->displayValues.empty();
    if (encodedFactor && !missing) {
        auto level = std::find(column->definedLevels.begin(), column->definedLevels.end(), stored);
        if (level == column->definedLevels.end()) {
            column->definedLevels.push_back(stored);
            level = column->definedLevels.end() - 1;
        }
        column->values[row] = std::to_string(
            static_cast<std::size_t>(level - column->definedLevels.begin()) + 1);
    } else {
        column->values[row] = stored;
    }
    if (!column->displayValues.empty()) {
        if (column->displayValues.size() < static_cast<std::size_t>(df.rows)) {
            column->displayValues.resize(static_cast<std::size_t>(df.rows), "NA");
        }
        column->displayValues[row] = stored;
    }
    if (imputedCell) {
        const std::size_t version = static_cast<std::size_t>(
            std::max(1, std::min(df.activeImputationVersion, df.imputationCount)) - 1);
        if (version < column->imputationValues.size() &&
            row < column->imputationValues[version].size())
            column->imputationValues[version][row] = column->values[row];
        if (column->imputationValuesSparse.size() <= version)
            column->imputationValuesSparse.resize(version + 1);
        column->imputationValuesSparse[version][row] = column->values[row];
    }
    if (!missing && VariableTypeIsFactorLike(type) && !encodedFactor &&
        std::find(column->definedLevels.begin(), column->definedLevels.end(), stored) == column->definedLevels.end()) {
        column->definedLevels.push_back(stored);
    }
    column->binary = DataColumnIsBinaryCategorical(*column);
    if (message) {
        *message = "Updated row " + std::to_string(row + 1) + ", variable `" + variable + "`.";
    }
    EnsureDataFrameProvenance(df);
    TransformationStep step;
    step.label = "Manual edit of " + variable;
    step.origin = RCodeOrigin::Unavailable;
    step.inputColumns = {variable};
    step.outputColumns = {variable};
    if (row < df.stableRowIds.size()) step.stableRowIds = {df.stableRowIds[row]};
    step.parameters["value"] = value;
    RecordDataFrameTransformation(df, std::move(step));
    return true;
}

bool SetDataColumnDecimals(DataColumn &col, int decimals, std::string *message)
{
    if (decimals < -1 || decimals > 12) {
        if (message) {
            *message = "Decimals must be between 0 and 12, or blank for automatic.";
        }
        return false;
    }
    col.decimals = decimals;
    return true;
}

bool IsValidVariableName(const std::string &name, std::string *message)
{
    if (name.empty()) {
        if (message) *message = "Variable name cannot be empty.";
        return false;
    }
    if (name.find('|') != std::string::npos || name.find('\t') != std::string::npos ||
        name.find('\n') != std::string::npos || name.find('\r') != std::string::npos) {
        if (message) *message = "Variable names cannot contain tabs, line breaks, or `|`.";
        return false;
    }
    return true;
}

std::string SafeDataColumnSuffix(const std::string &text)
{
    std::string out;
    for (unsigned char ch : text) {
        if (std::isalnum(ch)) {
            out.push_back(static_cast<char>(std::tolower(ch)));
        } else if (ch == '_' || ch == '-' || std::isspace(ch)) {
            if (out.empty() || out.back() != '_') {
                out.push_back('_');
            }
        }
    }
    while (!out.empty() && out.back() == '_') {
        out.pop_back();
    }
    if (out.empty()) {
        out = "plot";
    }
    if (out.size() > 32) {
        out.resize(32);
    }
    while (!out.empty() && out.back() == '_') {
        out.pop_back();
    }
    return out.empty() ? "plot" : out;
}

std::string SafeDatasetName(const std::string &text)
{
    std::string cleaned;
    bool previousSpace = false;
    for (char ch : text) {
        unsigned char c = static_cast<unsigned char>(ch);
        bool keep = std::isalnum(c) || ch == '_' || ch == '.' || ch == '-';
        if (keep) {
            cleaned.push_back(ch);
            previousSpace = false;
        } else if (!previousSpace) {
            cleaned.push_back(' ');
            previousSpace = true;
        }
    }
    while (!cleaned.empty() && cleaned.front() == ' ') {
        cleaned.erase(cleaned.begin());
    }
    while (!cleaned.empty() && cleaned.back() == ' ') {
        cleaned.pop_back();
    }
    return cleaned.empty() ? "dataset" : cleaned;
}

std::string SafeDatasetNameForPath(const std::string &path)
{
    std::string base = path;
    std::size_t slash = base.find_last_of("/\\");
    if (slash != std::string::npos) {
        base = base.substr(slash + 1);
    }
    std::size_t dot = base.find_last_of('.');
    if (dot != std::string::npos && dot > 0) {
        base = base.substr(0, dot);
    }
    return SafeDatasetName(base);
}

std::string UniqueDatasetName(const std::string &baseName,
                              const std::vector<std::string> &existingNames)
{
    std::string base = SafeDatasetName(baseName);
    std::set<std::string> existing(existingNames.begin(), existingNames.end());
    std::string candidate = base;
    int index = 2;
    while (existing.find(candidate) != existing.end()) {
        candidate = base + " " + std::to_string(index++);
    }
    return candidate;
}

std::string UniqueDataColumnName(const DataFrameModel &df, const std::string &base)
{
    std::set<std::string> existing;
    for (const DataColumn &col : df.columns) {
        existing.insert(col.name);
    }
    std::string candidate = base.empty() ? "column" : base;
    int suffix = 2;
    while (existing.find(candidate) != existing.end()) {
        candidate = (base.empty() ? "column" : base) + "_" + std::to_string(suffix++);
    }
    return candidate;
}

bool DerivedDataColumnKindIsSupported(const std::string &kind)
{
    return kind == "selection" || kind == "color";
}

bool AddDerivedDataColumn(DataFrameModel &df,
                          const std::string &source,
                          const std::string &kind,
                          const std::set<int> &selectedRows,
                          const std::map<int, std::string> &rowColors,
                          std::string *createdName,
                          std::string *message)
{
    if (!DerivedDataColumnKindIsSupported(kind)) {
        if (message) {
            *message = "Derived column kind must be selection or color.";
        }
        return false;
    }
    std::string safeSource = source.empty() ? "dataset" : source;
    std::string base = (kind == "selection" ? "selected_from_" : "point_color_from_") +
        SafeDataColumnSuffix(safeSource);

    DataColumn col;
    col.name = UniqueDataColumnName(df, base);
    col.displayName = col.name;
    col.type = "factor";
    col.decimals = -1;
    col.description = kind == "selection"
        ? "Selection state exported from LinkEDA plot `" + safeSource + "`."
        : "Point color state exported from LinkEDA plot `" + safeSource + "`.";
    col.values.reserve(static_cast<std::size_t>(std::max(0, df.rows)));

    for (int row = 1; row <= df.rows; ++row) {
        if (kind == "selection") {
            bool selected = selectedRows.find(row) != selectedRows.end();
            col.values.push_back(selected ? "selected" : "not_selected");
        } else {
            std::string color = "default";
            auto hit = rowColors.find(row);
            if (hit != rowColors.end() && !hit->second.empty()) {
                color = hit->second;
            }
            col.values.push_back(color);
        }
    }

    if (createdName) {
        *createdName = col.name;
    }
    df.columns.push_back(col);
    EnsureDataFrameProvenance(df);
    TransformationStep step;
    step.label = kind == "selection" ? "Add selection indicator column" : "Add point-colour column";
    step.origin = RCodeOrigin::Recorded;
    step.outputColumns = {col.name};
    if (kind == "selection") {
        std::vector<std::string> ids;
        for (int row : selectedRows) {
            if (row > 0 && static_cast<std::size_t>(row) <= df.stableRowIds.size())
                ids.push_back(df.stableRowIds[static_cast<std::size_t>(row - 1)]);
        }
        std::ostringstream selected;
        selected << "c(";
        for (std::size_t i = 0; i < ids.size(); ++i) {
            if (i) selected << ", ";
            selected << ProvenanceRStringLiteral(ids[i]);
        }
        selected << ")";
        step.rCode = RNameLiteral(col.name) +
            " <- factor(LinkEDA::ls_row_ids(data) %in% " + selected.str() +
            ", levels = c(FALSE, TRUE), labels = c(\"not_selected\", \"selected\"))";
    } else {
        step.rCode =
            "# Point colours were assigned interactively in LinkEDA.\n"
            "# The original native colour-state operation is preserved in the data snapshot,\n"
            "# but no equivalent R operation was executed.";
    }
    RecordDataFrameTransformation(df, std::move(step));
    return true;
}

bool BuildMissingDataPatternColumn(const DataFrameModel &df,
                                   const std::vector<std::string> &variables,
                                   DataColumn &column,
                                   std::string *message)
{
    if (variables.empty()) {
        if (message) *message = "Select at least one variable.";
        return false;
    }

    std::vector<const DataColumn *> selected;
    selected.reserve(variables.size());
    for (const std::string &variable : variables) {
        const DataColumn *found = FindDataColumnInDataFrame(df, variable);
        if (!found) {
            if (message) *message = VariableNotFoundStatus(variable);
            return false;
        }
        selected.push_back(found);
    }

    column = DataColumn{};
    column.name = UniqueDataColumnName(df, "missing_pattern");
    column.displayName = column.name;
    column.type = "factor";
    column.decimals = -1;
    column.description = "Missing data patterns based on selected variables.";
    column.values.reserve(static_cast<std::size_t>(std::max(0, df.rows)));

    std::map<std::string, int> patternIndex;
    int nextNumber = 1;
    for (int row = 0; row < df.rows; ++row) {
        std::string key;
        for (std::size_t index = 0; index < selected.size(); ++index) {
            const DataColumn &candidate = *selected[index];
            const std::size_t dataRow = static_cast<std::size_t>(row);
            if (dataRow >= candidate.values.size() ||
                !DataCellIsMissing(candidate.values[dataRow])) continue;
            const std::string &name = variables[index];
            if (!name.empty())
                key.push_back(static_cast<char>(std::tolower(
                    static_cast<unsigned char>(name.front()))));
        }
        if (key.empty()) {
            column.values.push_back("0");
            continue;
        }
        auto [position, inserted] = patternIndex.emplace(key, nextNumber);
        if (inserted) ++nextNumber;
        column.values.push_back(key + std::to_string(position->second));
    }
    if (message) message->clear();
    return true;
}

std::string VariableInformationText(const DataFrameModel &df,
                                    const std::string &variable)
{
    const DataColumn *col = FindDataColumnInDataFrame(df, variable);
    if (!col) {
        return "Variable `" + variable + "` was not found in dataset `" + df.group + "`.";
    }

    int missing = 0;
    std::set<std::string> uniqueValues;
    for (const std::string &value : col->values) {
        if (DataCellIsMissing(value)) {
            ++missing;
        } else if (uniqueValues.size() < 12) {
            uniqueValues.insert(value);
        }
    }

    std::ostringstream out;
    out << "Variable: " << col->name
        << "\nDisplay name: " << (col->displayName.empty() ? col->name : col->displayName)
        << "\nDataset: " << df.group;
    if (df.datasetType == "multiple_imputation" && df.imputationCount > 0) {
        const int count = std::max(1, df.imputationCount);
        const int active = std::max(1, std::min(count, df.activeImputationVersion));
        out << "\nDataset type: Multiple imputation"
            << "\nImputations: m = " << count
            << "\nData displayed: ";
        if (df.imputationDisplayMode == "all") {
            out << "Compact preview across all " << count << " imputations";
        } else if (df.imputationDisplayMode == "original") {
            out << "Original incomplete data";
        } else {
            out << "Imputation " << active << " of " << count;
        }
    } else {
        out << "\nDataset type: Ordinary data";
    }
    out << "\nStatistical type: " << VariableTypeDisplayName(col->type)
        << "\nR storage: " << DataColumnStorageType(*col)
        << "\nBinary: " << (DataColumnIsBinaryCategorical(*col) ? "Yes" : "No")
        << "\nCategories: " << (VariableTypeIsCategorical(col->type)
            ? JoinValues(DataColumnFactorLevels(*col), " / ") : "(not categorical)")
        << "\nDescription: " << (col->description.empty() ? "(none)" : col->description)
        << "\nDisplayed decimals: " << (col->decimals < 0 ? "automatic" : std::to_string(col->decimals))
        << "\nRows: " << col->values.size()
        << "\nMissing values: " << missing
        << "\nDistinct preview: ";
    if (uniqueValues.empty()) {
        out << "(none)";
    } else {
        std::size_t i = 0;
        for (const std::string &value : uniqueValues) {
            if (i++) {
                out << ", ";
            }
            out << value;
        }
    }
    return out.str();
}

bool RenameDataFrameColumn(DataFrameModel &df,
                           const std::string &oldName,
                           const std::string &newName,
                           std::string *message)
{
    if (!IsValidVariableName(newName, message)) {
        return false;
    }
    if (oldName == newName) {
        if (message) *message = "Variable name unchanged.";
        return true;
    }
    if (FindDataColumnInDataFrame(df, newName)) {
        if (message) *message = "Variable `" + newName + "` already exists.";
        return false;
    }
    DataColumn *col = FindDataColumnInDataFrame(df, oldName);
    if (!col) {
        if (message) *message = "Variable `" + oldName + "` was not found.";
        return false;
    }
    col->name = newName;
    if (col->displayName == oldName || col->displayName.empty()) {
        col->displayName = newName;
    }
    EnsureDataFrameProvenance(df);
    TransformationStep step;
    step.label = "Rename column " + oldName + " to " + newName;
    step.origin = RCodeOrigin::Recorded;
    step.inputColumns = {oldName};
    step.outputColumns = {newName};
    step.rCode = "names(data)[names(data) == " + ProvenanceRStringLiteral(oldName) + "] <- " +
        ProvenanceRStringLiteral(newName);
    RecordDataFrameTransformation(df, std::move(step));
    auto previous = df.provenance.columnSteps.find(oldName);
    if (previous != df.provenance.columnSteps.end()) {
        auto &target = df.provenance.columnSteps[newName];
        target.insert(target.begin(), previous->second.begin(), previous->second.end());
        df.provenance.columnSteps.erase(previous);
    }
    return true;
}

void EnsureDataFrameProvenance(DataFrameModel &df, RCodeOrigin origin,
                               const std::string &originCode,
                               const std::string &originDescription)
{
    if (df.dataVersion == 0) df.dataVersion = 1;
    if (df.stableRowIds.size() != static_cast<std::size_t>(std::max(0, df.rows)))
        df.stableRowIds = StableRowIdsForCount(df.group, static_cast<std::size_t>(std::max(0, df.rows)));
    if (df.provenance.currentVersion.datasetId.empty()) {
        df.provenance.origin = origin;
        df.provenance.originCode = originCode;
        df.provenance.originDescription = originDescription;
    }
    RefreshCurrentVersion(df);
}

void RecordDataFrameTransformation(DataFrameModel &df, TransformationStep step)
{
    EnsureDataFrameProvenance(df);
    if (step.id.empty()) step.id = NextTransformationId(df);
    if (step.parentVersionKeys.empty())
        step.parentVersionKeys.push_back(df.provenance.currentVersion.storageKey);
    df.provenance.history.push_back(step);
    for (const std::string &column : step.outputColumns)
        df.provenance.columnSteps[column].push_back(step.id);
    ++df.dataVersion;
    RefreshCurrentVersion(df);
}

void RecordDataFrameMetadataChange(DataFrameModel &df,
                                   const std::string &column,
                                   const std::string &label)
{
    TransformationStep step;
    step.label = label;
    step.origin = RCodeOrigin::Unavailable;
    step.inputColumns = {column};
    step.outputColumns = {column};
    RecordDataFrameTransformation(df, std::move(step));
}

std::vector<VariableViewRow> VariableViewRowsForDataFrame(const DataFrameModel &df)
{
    std::vector<VariableViewRow> rows;
    rows.reserve(df.columns.size());
    for (const DataColumn &col : df.columns) {
        rows.push_back({col.name, col.type, col.description, col.decimals});
    }
    return rows;
}

DataColumn *FindDataColumnInDataFrame(DataFrameModel &df, const std::string &name)
{
    for (DataColumn &col : df.columns) {
        if (col.name == name) {
            return &col;
        }
    }
    return nullptr;
}

const DataColumn *FindDataColumnInDataFrame(const DataFrameModel &df, const std::string &name)
{
    for (const DataColumn &col : df.columns) {
        if (col.name == name) {
            return &col;
        }
    }
    return nullptr;
}

bool DataFrameCellIsImputed(const DataFrameModel &df, const DataColumn &col, std::size_t row)
{
    return df.datasetType == "multiple_imputation" &&
        row < col.imputedMissing.size() &&
        col.imputedMissing[row];
}

int ImputationVersionCountForCell(const DataFrameModel &df, const DataColumn &col)
{
    int count = df.imputationCount;
    count = std::max(count, static_cast<int>(col.imputationValues.size()));
    count = std::max(count, static_cast<int>(col.imputationValuesSparse.size()));
    return count;
}

std::string OriginalImputationValueForCell(const DataColumn &col, std::size_t row)
{
    if (row < col.imputationOriginalValues.size()) {
        return col.imputationOriginalValues[row];
    }
    auto found = col.imputationOriginalSparse.find(row);
    if (found != col.imputationOriginalSparse.end()) {
        return found->second;
    }
    return "NA";
}

std::string DisplayValueForCell(const DataColumn &col, std::size_t row)
{
    if (row < col.displayValues.size()) {
        return col.displayValues[row];
    }
    if (row < col.values.size()) {
        if (col.decimals >= 0 && col.type == "numeric") {
            double parsed = NAN;
            if (ParseDataCellDouble(col.values[row], parsed)) {
                std::ostringstream out;
                out << std::fixed << std::setprecision(std::max(0, std::min(12, col.decimals))) << parsed;
                return out.str();
            }
        }
        return col.values[row];
    }
    return "";
}

std::vector<std::string> DisplayValuesForColumnsAtRow(
    const std::vector<const DataColumn *> &columns,
    std::size_t row,
    const std::string &missingValue)
{
    std::vector<std::string> values;
    for (const DataColumn *col : columns) {
        if (!col) {
            continue;
        }
        std::string value = DisplayValueForCell(*col, row);
        values.push_back(DataCellIsMissing(value) ? missingValue : value);
    }
    if (values.empty()) {
        values.push_back(missingValue);
    }
    return values;
}

std::map<int, std::string> RowLabelMapForColumn(const DataColumn &col)
{
    std::map<int, std::string> labels;
    const std::size_t n = std::max(col.values.size(), col.displayValues.size());
    for (std::size_t i = 0; i < n; ++i) {
        std::string value = DisplayValueForCell(col, i);
        if (!DataCellIsMissing(value)) {
            labels[static_cast<int>(i) + 1] = value;
        }
    }
    return labels;
}

std::string ImputationVersionValueForCell(const DataColumn &col,
                                          std::size_t row,
                                          std::size_t versionIndex)
{
    if (versionIndex < col.imputationValues.size() &&
        row < col.imputationValues[versionIndex].size()) {
        return col.imputationValues[versionIndex][row];
    }
    if (versionIndex < col.imputationValuesSparse.size()) {
        auto found = col.imputationValuesSparse[versionIndex].find(row);
        if (found != col.imputationValuesSparse[versionIndex].end()) {
            return found->second;
        }
    }
    return DisplayValueForCell(col, row);
}

std::string DisplayValueForDataFrameCell(const DataFrameModel &df,
                                         const DataColumn &col,
                                         std::size_t row)
{
    if (DataFrameCellIsImputed(df, col, row)) {
        if (df.imputationDisplayMode == "all") {
            static const std::size_t kMaxInlineImputationValues = 8;
            static const std::size_t kMaxInlineImputationChars = 180;
            std::size_t count = static_cast<std::size_t>(std::max(0, ImputationVersionCountForCell(df, col)));
            if (count == 0) {
                return "";
            }
            std::size_t shown = std::min(count, kMaxInlineImputationValues);
            std::vector<std::string> values;
            values.reserve(shown);
            for (std::size_t i = 0; i < shown; ++i) {
                values.push_back(ImputationVersionValueForCell(col, row, i));
            }
            std::string text = JoinValues(values, " | ");
            if (count > shown) {
                text += " | ... (" + std::to_string(count) + " imputations)";
            }
            return LimitInlineCellText(text, kMaxInlineImputationChars);
        }
        if (df.imputationDisplayMode == "original") {
            return OriginalImputationValueForCell(col, row);
        }
        int count = ImputationVersionCountForCell(df, col);
        int index = std::max(1, std::min(df.activeImputationVersion, count)) - 1;
        if (index >= 0) {
            return ImputationVersionValueForCell(col, row, static_cast<std::size_t>(index));
        }
    }
    return DisplayValueForCell(col, row);
}

void ApplyImputationDisplayModeToStoredValues(DataFrameModel &df)
{
    if (df.datasetType != "multiple_imputation" || df.imputationCount <= 0) {
        return;
    }
    int activeIndex = std::max(1, std::min(df.activeImputationVersion, df.imputationCount)) - 1;
    for (DataColumn &col : df.columns) {
        for (std::size_t row = 0; row < col.values.size(); ++row) {
            if (!DataFrameCellIsImputed(df, col, row)) {
                continue;
            }
            if (df.imputationDisplayMode == "original") {
                col.values[row] = OriginalImputationValueForCell(col, row);
            } else {
                col.values[row] = ImputationVersionValueForCell(col, row, static_cast<std::size_t>(std::max(0, activeIndex)));
            }
        }
    }
}

double NumericValueForDataFrameCellVersion(const DataFrameModel &df,
                                           const DataColumn &col,
                                           std::size_t row,
                                           int versionIndex)
{
    std::string text;
    if (DataFrameCellIsImputed(df, col, row)) {
        text = ImputationVersionValueForCell(col, row, static_cast<std::size_t>(std::max(0, versionIndex)));
    } else if (row < col.values.size()) {
        text = col.values[row];
    } else {
        text = DisplayValueForCell(col, row);
    }
    double value = NAN;
    return ParseDataCellDouble(text, value) ? value : NAN;
}

std::vector<int> CompleteRowsForDataColumns(const DataFrameModel &df,
                                            const std::vector<const DataColumn *> &columns,
                                            int versionIndex)
{
    std::vector<int> rows;
    if (columns.empty()) {
        return rows;
    }
    for (int row = 0; row < df.rows; ++row) {
        bool complete = true;
        for (const DataColumn *col : columns) {
            if (!col || !std::isfinite(NumericValueForDataFrameCellVersion(df, *col, static_cast<std::size_t>(row), versionIndex))) {
                complete = false;
                break;
            }
        }
        if (complete) {
            rows.push_back(row + 1);
        }
    }
    return rows;
}

std::vector<int> CompleteRowsForNumericVectors(const std::vector<std::vector<double>> &columns)
{
    std::vector<int> rows;
    if (columns.empty()) {
        return rows;
    }
    std::size_t n = static_cast<std::size_t>(-1);
    for (const std::vector<double> &column : columns) {
        n = std::min(n, column.size());
    }
    if (n == static_cast<std::size_t>(-1)) {
        return rows;
    }
    for (std::size_t i = 0; i < n; ++i) {
        bool complete = true;
        for (const std::vector<double> &column : columns) {
            if (!std::isfinite(column[i])) {
                complete = false;
                break;
            }
        }
        if (complete) {
            rows.push_back(static_cast<int>(i) + 1);
        }
    }
    return rows;
}

bool ScatterImputationUncertaintyModeIsValid(const std::string &mode)
{
    return mode == "central80" || mode == "iqr" || mode == "sd" || mode == "se";
}

std::string NormalizedScatterImputationUncertaintyMode(const std::string &mode)
{
    return ScatterImputationUncertaintyModeIsValid(mode) ? mode : "central80";
}

std::string ScatterImputationUncertaintyDisplayName(const std::string &mode)
{
    std::string normalized = NormalizedScatterImputationUncertaintyMode(mode);
    if (normalized == "iqr") return "IQR";
    if (normalized == "sd") return "Mean +/- 1 SD";
    if (normalized == "se") return "Mean +/- 1 SE";
    return "Central 80% interval";
}

void NumericRangeInclude(NumericImputationRange &range, double value)
{
    if (!std::isfinite(value)) {
        return;
    }
    if (!range.any) {
        range.min = value;
        range.max = value;
        range.any = true;
    } else {
        range.min = std::min(range.min, value);
        range.max = std::max(range.max, value);
    }
}

NumericImputationRange NumericImputationRangeForValues(
    std::vector<double> values,
    const std::string &uncertaintyMode)
{
    NumericImputationRange range;
    values.erase(std::remove_if(values.begin(), values.end(), [](double value) {
        return !std::isfinite(value);
    }), values.end());
    if (values.empty()) {
        return range;
    }

    double sum = 0.0;
    for (double value : values) {
        sum += value;
    }
    double mean = sum / static_cast<double>(values.size());
    double halfWidth = 0.0;
    std::string mode = NormalizedScatterImputationUncertaintyMode(uncertaintyMode);
    if (mode == "iqr") {
        halfWidth = (QuantileValue(values, 0.75) - QuantileValue(values, 0.25)) / 2.0;
    } else if (mode == "sd" || mode == "se") {
        double sumSquares = 0.0;
        for (double value : values) {
            double delta = value - mean;
            sumSquares += delta * delta;
        }
        double sd = values.size() > 1
            ? std::sqrt(sumSquares / static_cast<double>(values.size() - 1))
            : 0.0;
        halfWidth = mode == "se" ? sd / std::sqrt(static_cast<double>(values.size())) : sd;
    } else {
        halfWidth = (QuantileValue(values, 0.90) - QuantileValue(values, 0.10)) / 2.0;
    }

    range.center = mean;
    range.min = mean - std::max(0.0, halfWidth);
    range.max = mean + std::max(0.0, halfWidth);
    if (!std::isfinite(range.min) || !std::isfinite(range.max) || !std::isfinite(range.center)) {
        return NumericImputationRange();
    }
    if (range.min > range.max) {
        std::swap(range.min, range.max);
    }
    range.any = true;
    return range;
}

NumericImputationRange NumericImputationRangeForCell(const DataFrameModel &df,
                                                     const DataColumn &col,
                                                     std::size_t row,
                                                     const std::string &uncertaintyMode)
{
    const bool markedImputed = DataFrameCellIsImputed(df, col, row);
    const int count = ImputationVersionCountForCell(df, col);
    std::vector<double> values;
    values.reserve(static_cast<std::size_t>(std::max(0, count)));
    for (int version = 0; version < count; ++version) {
        double value = NAN;
        if (ParseDataCellDouble(ImputationVersionValueForCell(
                col, row, static_cast<std::size_t>(version)), value)) {
            values.push_back(value);
        }
    }
    if (!markedImputed && !values.empty()) {
        const auto limits = std::minmax_element(values.begin(), values.end());
        if (limits.first != values.end() &&
            std::fabs(*limits.second - *limits.first) <= 1.0e-12) {
            return NumericImputationRange();
        }
    }
    return NumericImputationRangeForValues(std::move(values), uncertaintyMode);
}

bool DataFrameShowsAllImputations(const DataFrameModel &df)
{
    return df.datasetType == "multiple_imputation" &&
        df.imputationCount > 0 &&
        df.imputationDisplayMode == "all";
}

std::string PlotImputationDisplayStatus(const DataFrameModel &df,
                                        bool summarizesAllImputations)
{
    if (df.datasetType != "multiple_imputation" || df.imputationCount <= 0) {
        return "";
    }
    const int count = std::max(1, df.imputationCount);
    const int active = std::max(1, std::min(count, df.activeImputationVersion));
    if (df.imputationDisplayMode == "original") {
        return "Showing original incomplete data (not pooled).";
    }
    if (df.imputationDisplayMode == "all") {
        if (summarizesAllImputations) {
            return "All " + std::to_string(count) +
                " imputations selected; points show imputation " +
                std::to_string(active) +
                " and uncertainty glyphs summarize all imputations.";
        }
        return "All imputations selected in the Data Sheet; this descriptive plot shows imputation " +
            std::to_string(active) + " of " + std::to_string(count) + " (not pooled).";
    }
    return "Showing imputation " + std::to_string(active) + " of " +
        std::to_string(count) + " (not pooled).";
}

std::string PooledEffectPlotImputationStatus(const DataFrameModel &df)
{
    if (df.datasetType != "multiple_imputation" || df.imputationCount <= 0) {
        return "";
    }
    return "Effect estimates are pooled across all " +
        std::to_string(std::max(1, df.imputationCount)) +
        " imputations; no single imputation is displayed.";
}

std::string DataFrameStatusText(const DataFrameModel &df,
                                std::size_t selectedCount)
{
    std::string status = df.group + ": " + std::to_string(df.rows) + " rows, " +
        std::to_string(df.columns.size()) + " variables, " +
        std::to_string(selectedCount) + " selected";
    if (df.datasetType == "multiple_imputation") {
        if (df.imputationDisplayMode == "all") {
            status += " | Showing compact all-imputation preview across " +
                std::to_string(df.imputationCount) + " imputations";
        } else if (df.imputationDisplayMode == "original") {
            status += " | Showing original incomplete data";
        } else {
            status += " | Showing imputation: " + std::to_string(df.activeImputationVersion) +
                " of " + std::to_string(df.imputationCount);
        }
    }
    return status;
}

std::string DataFrameImputationTooltipText(const DataFrameModel &df,
                                           const DataColumn &col,
                                           std::size_t row)
{
    if (df.datasetType != "multiple_imputation" ||
        !DataFrameCellIsImputed(df, col, row)) {
        return "";
    }
    if (df.imputationDisplayMode == "all") {
        return "Originally missing. Showing a compact preview across " +
            std::to_string(df.imputationCount) + " imputations.";
    }
    if (df.imputationDisplayMode == "original") {
        return "Originally missing. Showing original incomplete data.";
    }
    return "Originally missing. Imputed value in imputation " +
        std::to_string(df.activeImputationVersion) + ".";
}

std::string DataFrameWindowTitle(const std::string &group)
{
    return "Data Sheet - " + group;
}

std::string DataSheetChooseDataColumnStatus()
{
    return "Choose a data column, not the row-number column.";
}

std::string VariableInformationDialogTitle()
{
    return "Variable Information";
}

std::string NoActiveDatasetTitle()
{
    return "No Active Dataset";
}

std::string ActiveDatasetTitle()
{
    return "Active Dataset";
}

std::string ActiveDatasetUnavailableStatus()
{
    return "The active dataset is no longer available.";
}

std::string ActiveDatasetSummaryText(const std::string &group,
                                     int rows,
                                     std::size_t variableCount,
                                     std::size_t selectedRows)
{
    std::ostringstream msg;
    msg << "Active dataset: " << group << "\n"
        << "Rows: " << rows << "\n"
        << "Variables: " << variableCount << "\n"
        << "Selected rows: " << selectedRows;
    return msg.str();
}

std::string ImportOrRegisterDatasetStatus()
{
    return "Import or register a dataset first.";
}

std::string NoActiveDatasetImportOrRegisterStatus()
{
    return "No active dataset. Import or register a dataset first.";
}

std::string NoActiveDatasetStatus()
{
    return "No active dataset is available.";
}

std::string NoRegisteredDatasetGroupStatus()
{
    return "No registered dataset/group is available.";
}

std::string DatasetNotAvailableStatus()
{
    return "The dataset is not available.";
}

std::string DatasetColumnNotFoundStatus(const std::string &column,
                                        const std::string &group)
{
    return "Column `" + column + "` was not found in dataset `" + group + "`.";
}

std::string DatasetVariableNameUnchangedStatus()
{
    return "Variable name unchanged.";
}

std::string DatasetPointLabelsResetStatus()
{
    return "Point labels reset.";
}

std::string DatasetPointLabelColumnSetStatus(const std::string &column)
{
    return "Point label column set to `" + column + "`.";
}

std::string DatasetTemporaryDataWriteFailedStatus()
{
    return "Could not write temporary GLM data.";
}

std::string VariableViewTitle()
{
    return "Variable View";
}

std::string VariableViewWindowTitle(const std::string &group)
{
    return VariableViewTitle() + " - " + group;
}

std::string VariableViewInstructionText()
{
    return "Click Name or Description to edit. Click Type or Decimals to choose an action.";
}

std::string VariableDescriptionDialogTitle(const std::string &variable)
{
    return "Description for " + variable;
}

std::string VariableDescriptionDialogInformationText()
{
    return "Edit the explanatory text used by Variable Information and the Variable View.";
}

std::string DatasetNotRegisteredStatus(const std::string &group)
{
    return "Dataset `" + group + "` is not registered.";
}

std::string DataSheetOpenTitle()
{
    return "Open Data Sheet";
}

std::string DataSheetBackendPayloadUnavailableStatus()
{
    return "No backend data payload is available for this dataset yet. Create a new plot from R so the data can be sent to the native workbench.";
}

std::string VariableTypeTitle()
{
    return "Variable Type";
}

std::string NoActiveVariableForTypeStatus()
{
    return "No active variable is available. Open a data sheet and right-click a column.";
}

std::string VariableDecimalsInfoText()
{
    return "Use automatic formatting or choose a fixed number of displayed decimals.";
}

std::string NoRowsSelectedStatus()
{
    return "No rows selected.";
}

std::string NoCompleteCasesStatus()
{
    return "No complete cases for the selected variables and missing-data mode.";
}

std::string DatasetForPlotNotAvailableStatus()
{
    return "The dataset for this plot is not available.";
}

std::string VariableForPlotNotAvailableStatus()
{
    return "The variable for this plot is not available.";
}

std::string SelectedVariableNotAvailableStatus()
{
    return "The selected variable is not available.";
}

std::string NoVariablesToSummarizeStatus()
{
    return "This plot does not have variables that can be summarized.";
}

std::string Table1HistogramRequiresNumericStatus()
{
    return "Histogram requires a numeric or ordinal variable.";
}

std::string Table1BarplotRequiresValueStatus()
{
    return "Bar chart requires at least one observed value.";
}

std::string Table1BoxplotRequiresNumericStatus()
{
    return "Boxplot requires a numeric or ordinal variable.";
}

std::string DatasetNotAvailableForRecomputeStatus()
{
    return "Dataset is not available for native recompute.";
}

std::string NativeBackendCannotInspectRStatus()
{
    return "The native backend cannot inspect R environments directly. Run ls_refresh_r_dataframes() from R, then use ls_set_active_dataset().";
}

std::string VariableDecimalsAutoButtonTitle()
{
    return "Automatic";
}

std::string ScopeAllDataTitle()
{
    return "All data";
}

std::string ScopeSelectedRowsTitle()
{
    return "Selected rows";
}

std::string ScopeUnselectedRowsTitle()
{
    return "Unselected rows";
}

std::string ScopeCompareSelectedAllTitle()
{
    return "Compare selected/all";
}

std::string ChooseLabelColumnStatusTitle()
{
    return "Label Column";
}

std::string AlertOKButtonTitle()
{
    return "OK";
}

std::string AlertApplyButtonTitle()
{
    return "Apply";
}

std::string AlertCancelButtonTitle()
{
    return "Cancel";
}

std::string RFileExtension()
{
    return "R";
}

std::string PDFFileExtension()
{
    return "PDF";
}

std::string PNGFileExtension()
{
    return "PNG";
}

ChooseLabelColumnDialogState BuildChooseLabelColumnDialogState()
{
    return ChooseLabelColumnDialogState{};
}

std::string DatasetDialogLabel(const std::string &group)
{
    return BuildChooseLabelColumnDialogState().datasetLabelPrefix + group;
}

std::string NoVariableOptionTitle()
{
    return BuildChooseLabelColumnDialogState().noneOptionTitle;
}

std::string ChooseLabelColumnDatasetLabel(const std::string &group)
{
    return DatasetDialogLabel(group);
}

MissingDataImputationDialogState BuildMissingDataImputationDialogState()
{
    return MissingDataImputationDialogState{};
}

std::string NativeImportDialogTitle()
{
    return "Import Data";
}

std::string NativeImportFailedTitle()
{
    return "Import Failed";
}

std::string NativeImportSupportedFormatsText()
{
    return "CSV, TSV/TXT, Excel (.xls/.xlsx), SPSS (.sav/.zsav), Stata (.dta), "
           "SAS (.sas7bdat/.xpt), and R (.rds/.rda/.RData) files.";
}

std::vector<std::string> NativeImportAllowedFileExtensions()
{
    return {
        "csv",
        "txt",
        "tsv",
        "sav",
        "zsav",
        "dta",
        "sas7bdat",
        "xpt",
        "xls",
        "xlsx",
        "rds",
        "rda",
        "RData"
    };
}

std::vector<NativeImportFileFilter> NativeImportFileFilters()
{
    const std::vector<std::string> all = NativeImportAllowedFileExtensions();
    return {
        {"all", "All supported data files", all},
        {"mice", "mice multiple-imputation files (.rds, .rda, .RData)",
            {"rds", "rda", "RData"}},
        {"spss", "SPSS files (.sav, .zsav)", {"sav", "zsav"}},
        {"delimited", "CSV and text files (.csv, .tsv, .txt)", {"csv", "tsv", "txt"}},
        {"excel", "Excel files (.xlsx, .xls)", {"xlsx", "xls"}},
        {"stata", "Stata files (.dta)", {"dta"}},
        {"sas", "SAS files (.sas7bdat, .xpt)", {"sas7bdat", "xpt"}},
        {"r", "R data files (.rds, .rda, .RData)", {"rds", "rda", "RData"}}
    };
}

std::vector<NativeDataExportFileFilter> NativeDataExportFileFilters()
{
    return {
        {"csv", "CSV data file (.csv)", "csv"},
        {"xlsx", "Excel workbook (.xlsx)", "xlsx"},
        {"sav", "SPSS data file (.sav)", "sav"},
        {"zsav", "Compressed SPSS data file (.zsav)", "zsav"},
        {"dta", "Stata data file (.dta)", "dta"},
        {"xpt", "SAS transport file (.xpt)", "xpt"},
        {"rds", "R serialized data file (.rds)", "rds"},
        {"rdata", "R workspace data file (.RData)", "RData"},
        {"tsv", "Tab-delimited text file (.tsv)", "tsv"}
    };
}

std::string NativeDataExportFormatForExtension(const std::string &extension)
{
    std::string normalized = LowerCopy(TrimCopy(extension));
    while (!normalized.empty() && normalized.front() == '.') normalized.erase(normalized.begin());
    if (normalized == "txt") return "tsv";
    if (normalized == "rda" || normalized == "rdata") return "rdata";
    for (const NativeDataExportFileFilter &filter : NativeDataExportFileFilters()) {
        if (LowerCopy(filter.extension) == normalized) return filter.identifier;
    }
    return {};
}

std::string NativeDataExportRScript()
{
    return R"RLSEXPORT(
args <- commandArgs(TRUE)
if (length(args) < 4L) stop("Expected input CSV, metadata, output path, and export format.", call. = FALSE)
input <- args[[1L]]
metadata_path <- args[[2L]]
output <- args[[3L]]
format <- tolower(args[[4L]])
if (!file.exists(input)) stop("The temporary export data are unavailable.", call. = FALSE)
data <- utils::read.csv(input, check.names = FALSE, stringsAsFactors = FALSE,
                        na.strings = "NA")
if (file.exists(metadata_path)) {
  metadata <- utils::read.csv(metadata_path, check.names = FALSE,
                              stringsAsFactors = FALSE, na.strings = NULL)
  for (variable in unique(metadata$variable)) {
    rows <- metadata[metadata$variable == variable, , drop = FALSE]
    type <- rows$type[[1L]]
    if (!variable %in% names(data) || !type %in% c("factor", "ordered")) next
    level_rows <- rows[rows$level_index > 0L, , drop = FALSE]
    level_rows <- level_rows[order(level_rows$level_index), , drop = FALSE]
    levels <- as.character(level_rows$level)
    if (!length(levels)) levels <- unique(data[[variable]][!is.na(data[[variable]])])
    data[[variable]] <- factor(data[[variable]], levels = levels,
                               ordered = identical(type, "ordered"))
  }
}
if (!requireNamespace("LinkEDA", quietly = TRUE)) {
  stop("The LinkEDA R package is not available to the export process.", call. = FALSE)
}
is_mi_long <- all(c(".imp", ".id") %in% names(data)) &&
  length(unique(data$.imp)) > 1L && any(data$.imp == 0L)
if (is_mi_long && format %in% c("rds", "rdata")) {
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop("RDS/RData multiple-imputation export requires package 'mice'.", call. = FALSE)
  }
  export_object <- mice::as.mids(data, .imp = ".imp", .id = ".id")
  attr(export_object, "linkeda_exported_dataset_type") <- "multiple_imputation"
  LinkEDA:::.rls_export_r_object(export_object, output, format)
} else {
  LinkEDA:::.rls_export_data_frame(data, output, format)
}
)RLSEXPORT";
}

std::string NativeDataExportTemporaryFileFailedStatus()
{
    return "Could not create the temporary files required for data export.";
}

std::string NativeDataExportRscriptLaunchFailedStatus()
{
    return "Could not start Rscript for data export.";
}

std::string NativeDataExportRscriptFailedStatus()
{
    return "R could not export the selected data format.";
}

std::string NativeDataExportSuccessStatus(const std::string &format,
                                          bool multipleImputation)
{
    std::string label = "data";
    const std::string normalized = LowerCopy(format);
    if (normalized == "csv") label = "CSV";
    else if (normalized == "tsv") label = "tab-delimited text";
    else if (normalized == "xlsx") label = "Excel";
    else if (normalized == "sav") label = "SPSS";
    else if (normalized == "zsav") label = "compressed SPSS";
    else if (normalized == "dta") label = "Stata";
    else if (normalized == "xpt") label = "SAS transport";
    else if (normalized == "rds") label = "RDS";
    else if (normalized == "rdata") label = "RData";
    if (multipleImputation) {
        if (normalized == "rds" || normalized == "rdata") {
            return "The multiple-imputation dataset was exported as a mice mids object in " +
                label + " format and can be reimported with its imputations preserved.";
        }
        return "The multiple-imputation dataset was exported as " + label +
            " in long format with .imp and .id columns.";
    }
    return "The active dataset was exported as " + label + ".";
}

std::string NativeImportTemporaryScriptFailedStatus()
{
    return "Could not create temporary import script.";
}

std::string NativeImportRscriptLaunchFailedStatus()
{
    return "Could not run Rscript for data import.";
}

std::string NativeImportRscriptFailedStatus()
{
    return "Rscript failed while importing the selected file.";
}

std::string NativeImportPayloadMissingStatus()
{
    return "The import reader did not return a dataset payload.";
}

std::string NativeImportDatasetLoadedStatus(const std::string &group)
{
    return "Imported `" + group + "`.";
}

std::string NativeImportDatasetLoadedStatus(const std::string &group,
                                            int rows,
                                            std::size_t variableCount)
{
    std::ostringstream out;
    out << "Imported `" << group << "` with " << rows
        << " rows and " << variableCount << " variables.";
    return out.str();
}

std::string ImportedVariableTypeReviewWarning(const DataFrameModel &dataframe)
{
    std::vector<std::string> warnings;
    for (const DataColumn &column : dataframe.columns) {
        const std::string type = NormalizeVariableType(column.type);
        if (type != "factor" && type != "ordered" && type != "character") continue;

        std::size_t observed = 0;
        std::size_t numeric = 0;
        std::vector<std::string> incompatibleExamples;
        for (std::size_t row = 0; row < column.values.size(); ++row) {
            const std::string value = UnformattedDisplayValue(column, row);
            if (DataCellIsMissing(value)) continue;
            ++observed;
            double parsed = NAN;
            if (ParseDataCellDouble(value, parsed)) {
                ++numeric;
            } else if (incompatibleExamples.size() < 3 &&
                       std::find(incompatibleExamples.begin(), incompatibleExamples.end(), value) ==
                           incompatibleExamples.end()) {
                incompatibleExamples.push_back(value);
            }
        }
        if (observed < 4 || numeric < 4 || numeric == observed ||
            static_cast<double>(numeric) / static_cast<double>(observed) < 0.90) {
            continue;
        }

        const std::size_t incompatible = observed - numeric;
        std::ostringstream warning;
        warning << "`" << column.name << "` looks numeric, but " << incompatible
                << " of " << observed << " non-missing "
                << (incompatible == 1 ? "value is" : "values are")
                << " not numeric";
        if (!incompatibleExamples.empty()) {
            warning << " (";
            for (std::size_t index = 0; index < incompatibleExamples.size(); ++index) {
                if (index) warning << ", ";
                warning << "`" << incompatibleExamples[index] << "`";
            }
            warning << ")";
        }
        warning << ". It was imported as "
                << (type == "factor" ? "Categorical" : type == "ordered" ? "Ordinal" : "Text")
                << "; review these values before changing it to Numeric.";
        warnings.push_back(warning.str());
    }
    if (warnings.empty()) return "";

    std::ostringstream out;
    out << "Review imported variable types: ";
    for (std::size_t index = 0; index < warnings.size(); ++index) {
        if (index) out << " ";
        out << warnings[index];
    }
    return out.str();
}

std::string NativeRDataPayloadTemporaryFileFailedStatus()
{
    return "Could not create temporary R data payload.";
}

std::string NativeMiceTemporaryScriptFailedStatus()
{
    return "Could not create temporary imputation script.";
}

std::string NativeMiceRscriptLaunchFailedStatus()
{
    return "Could not run Rscript for multiple imputation.";
}

std::string NativeMiceRscriptFailedStatus()
{
    return "Rscript failed while running multiple imputation.";
}

std::string NativeMicePayloadMissingStatus()
{
    return "The imputation script did not return a dataset payload.";
}

std::string NativeMiceCreatedDatasetStatus(const std::string &outputGroup, int m)
{
    return "Created imputed dataset `" + outputGroup + "` with " +
        std::to_string(std::max(1, m)) + " imputations.";
}

std::string NativePooledAnalysisTemporaryScriptFailedStatus()
{
    return "Could not create temporary R analysis script.";
}

std::string NativePooledAnalysisRscriptLaunchFailedStatus()
{
    return "Could not run Rscript for the multiple-imputation analysis.";
}

std::string NativePooledAnalysisRscriptFailedStatus()
{
    return "Rscript failed while computing the multiple-imputation analysis.";
}

std::string NativePooledAnalysisPayloadMissingStatus()
{
    return "The R/mice analysis did not return a native table payload.";
}

std::string NativePooledAnalysisOpenedStatus()
{
    return "Opened pooled multiple-imputation analysis.";
}

bool DatasetRegistry::empty() const
{
    return dataFrames_.empty();
}

std::size_t DatasetRegistry::size() const
{
    return dataFrames_.size();
}

bool DatasetRegistry::contains(const std::string &group) const
{
    return dataFrames_.find(group) != dataFrames_.end();
}

void DatasetRegistry::clear()
{
    dataFrames_.clear();
    activeGroup_.clear();
}

void DatasetRegistry::registerDataset(const DataFrameModel &df)
{
    if (df.group.empty()) {
        return;
    }
    DataFrameModel registered = df;
    EnsureDataFrameProvenance(registered);
    dataFrames_[df.group] = std::move(registered);
    if (activeGroup_.empty()) {
        activeGroup_ = df.group;
    }
}

bool DatasetRegistry::erase(const std::string &group)
{
    auto erased = dataFrames_.erase(group);
    if (!erased) {
        return false;
    }
    if (activeGroup_ == group) {
        activeGroup_ = dataFrames_.empty() ? "" : dataFrames_.begin()->first;
    }
    return true;
}

DataFrameModel *DatasetRegistry::find(const std::string &group)
{
    auto it = dataFrames_.find(group);
    return it == dataFrames_.end() ? nullptr : &it->second;
}

const DataFrameModel *DatasetRegistry::find(const std::string &group) const
{
    auto it = dataFrames_.find(group);
    return it == dataFrames_.end() ? nullptr : &it->second;
}

const std::map<std::string, DataFrameModel> &DatasetRegistry::datasets() const
{
    return dataFrames_;
}

bool DatasetRegistry::setActiveDataset(const std::string &group)
{
    if (!contains(group)) {
        return false;
    }
    activeGroup_ = group;
    return true;
}

void DatasetRegistry::rememberActiveDatasetGroup(const std::string &group)
{
    activeGroup_ = group;
}

std::string DatasetRegistry::activeDatasetGroup(const std::string &fallbackGroup) const
{
    if (!activeGroup_.empty() && contains(activeGroup_)) {
        return activeGroup_;
    }
    if (!fallbackGroup.empty() && contains(fallbackGroup)) {
        return fallbackGroup;
    }
    if (!dataFrames_.empty()) {
        return dataFrames_.begin()->first;
    }
    return "";
}

const DataFrameModel *DatasetRegistry::activeDataset(const std::string &fallbackGroup) const
{
    std::string group = activeDatasetGroup(fallbackGroup);
    return group.empty() ? nullptr : find(group);
}

std::vector<std::string> DatasetRegistry::datasetGroups() const
{
    std::vector<std::string> groups;
    groups.reserve(dataFrames_.size());
    for (const auto &entry : dataFrames_) {
        groups.push_back(entry.first);
    }
    return groups;
}

std::string DatasetRegistry::uniqueDatasetName(const std::string &baseName) const
{
    return UniqueDatasetName(baseName, datasetGroups());
}

bool DatasetRegistry::isMultipleImputation(const std::string &group) const
{
    const DataFrameModel *df = find(group);
    return df && df->datasetType == "multiple_imputation";
}

size_t DatasetRegistry::rowCount(const std::string &group) const
{
    const DataFrameModel *df = find(group);
    return df ? (size_t)std::max(0, df->rows) : 0;
}

std::string ActiveDatasetWindowTitle()
{
    return "Active Dataset";
}

std::string RefreshRDataFramesWindowTitle()
{
    return "Refresh R Data Frames";
}

std::string HashColumnHeader()
{
    return "#";
}

} // namespace core
} // namespace rlispstat
