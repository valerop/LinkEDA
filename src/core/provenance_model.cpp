#include "provenance_model.h"

#include <algorithm>
#include <cstddef>
#include <cctype>
#include <cmath>
#include <cstdlib>
#include <functional>
#include <iomanip>
#include <locale>
#include <set>
#include <sstream>

namespace rlispstat {
namespace core {
namespace {

std::string StorageKey(const std::string &datasetId, std::uint64_t version)
{
    return datasetId + "@" + std::to_string(std::max<std::uint64_t>(1, version));
}

std::string RCharacterVector(const std::vector<std::string> &values)
{
    if (values.empty()) return "character()";
    std::ostringstream out;
    out << "c(";
    for (std::size_t i = 0; i < values.size(); ++i) {
        if (i) out << ", ";
        out << ProvenanceRStringLiteral(values[i]);
    }
    out << ")";
    return out.str();
}

std::string RStringList(const std::vector<std::string> &values)
{
    if (values.empty()) return "NULL";
    std::ostringstream out;
    out << "list(";
    for (std::size_t i = 0; i < values.size(); ++i) {
        if (i) out << ", ";
        out << ProvenanceRStringLiteral(values[i]);
    }
    out << ")";
    return out.str();
}

std::string RNumber(double value)
{
    if (std::isnan(value)) return "NA_real_";
    if (std::isinf(value)) return value > 0 ? "Inf" : "-Inf";
    std::ostringstream out;
    out << std::setprecision(17) << value;
    return out.str();
}

std::string RValue(const PublicationValue &value)
{
    switch (value.kind) {
    case PublicationValueKind::Number: return RNumber(value.number);
    case PublicationValueKind::Text: return ProvenanceRStringLiteral(value.text);
    case PublicationValueKind::Logical: return value.logical ? "TRUE" : "FALSE";
    case PublicationValueKind::Missing:
    default: return "NA";
    }
}

bool PublicationValueIsEmpty(const PublicationValue &value)
{
    if (value.kind == PublicationValueKind::Missing) return true;
    if (value.kind == PublicationValueKind::Number) return std::isnan(value.number);
    if (value.kind == PublicationValueKind::Text) return value.text.empty();
    return false;
}

bool PublicationColumnHasContent(const PublicationTableSpec &spec,
                                 std::size_t column)
{
    return std::any_of(spec.rows.begin(), spec.rows.end(), [&](const auto &row) {
        return column < row.size() && !PublicationValueIsEmpty(row[column]);
    });
}

std::vector<const TransformationStep *> OrderedColumnSteps(
    const DataProvenance &provenance, const std::string &column)
{
    std::vector<const TransformationStep *> ordered;
    std::set<std::string> added;
    std::set<std::string> visiting;
    const auto findStep = [&](const std::string &id) -> const TransformationStep * {
        auto found = std::find_if(provenance.history.begin(), provenance.history.end(),
            [&](const TransformationStep &step) { return step.id == id; });
        return found == provenance.history.end() ? nullptr : &*found;
    };
    std::function<void(const std::string &)> addColumn;
    std::function<void(const std::string &)> addStep;
    addStep = [&](const std::string &id) {
        if (added.count(id) || visiting.count(id)) return;
        const TransformationStep *step = findStep(id);
        if (!step) return;
        visiting.insert(id);
        for (const std::string &input : step->inputColumns) addColumn(input);
        visiting.erase(id);
        if (added.insert(id).second) ordered.push_back(step);
    };
    addColumn = [&](const std::string &name) {
        auto found = provenance.columnSteps.find(name);
        if (found == provenance.columnSteps.end()) return;
        for (const std::string &id : found->second) addStep(id);
    };
    addColumn(column);
    return ordered;
}

void AppendOriginCode(std::ostringstream &out, RCodeOrigin origin,
                      const std::string &code, const std::string &description)
{
    if (origin == RCodeOrigin::Recorded && !code.empty()) {
        out << code;
        if (code.back() != '\n') out << '\n';
    } else if (origin == RCodeOrigin::Reconstructed && !code.empty()) {
        out << "# Reconstructed from object metadata.\n"
               "# This may not be identical to the original code.\n" << code;
        if (code.back() != '\n') out << '\n';
    } else {
        out << "# The original R code used to create these data is not available.\n"
               "# This data version is preserved through its LinkEDA snapshot.\n";
        if (!description.empty()) out << "# Source: " << description << "\n";
    }
}

} // namespace

std::string RCodeOriginId(RCodeOrigin origin)
{
    switch (origin) {
    case RCodeOrigin::Recorded: return "recorded";
    case RCodeOrigin::Reconstructed: return "reconstructed";
    case RCodeOrigin::Unavailable: default: return "unavailable";
    }
}

RCodeOrigin RCodeOriginFromId(const std::string &id)
{
    if (id == "recorded") return RCodeOrigin::Recorded;
    if (id == "reconstructed") return RCodeOrigin::Reconstructed;
    return RCodeOrigin::Unavailable;
}

DataVersionReference DefaultDataVersionReference(const std::string &datasetId,
                                                 const std::string &datasetType,
                                                 int imputationCount,
                                                 std::uint64_t version)
{
    DataVersionReference result;
    result.datasetId = datasetId;
    result.displayName = datasetId;
    result.version = std::max<std::uint64_t>(1, version);
    result.objectType = datasetType == "multiple_imputation" ? "mids" : "data.frame";
    result.imputationCount = std::max(0, imputationCount);
    result.storageKey = StorageKey(datasetId, result.version);
    return result;
}

std::vector<std::string> StableRowIdsForCount(const std::string &datasetId,
                                              std::size_t rowCount)
{
    std::vector<std::string> ids;
    ids.reserve(rowCount);
    for (std::size_t row = 0; row < rowCount; ++row)
        ids.push_back(datasetId + ":row:" + std::to_string(row + 1));
    return ids;
}

ImmutableAnalysisScope CaptureImmutableAnalysisScope(
    const AnalysisScope &scope, const DataVersionReference &version,
    const std::vector<std::string> &datasetStableRowIds,
    const std::vector<int> &effectiveOriginalRows,
    const std::vector<int> &excludedOriginalRows)
{
    ImmutableAnalysisScope result;
    result.kind = scope.kind == AnalysisScopeKind::AllObservations ? "all" : "explicit";
    result.sourceKind = AnalysisScopeSourceKindId(scope.sourceKind);
    result.description = scope.sourceDescription.empty()
        ? (result.kind == "all" ? "All observations" : "Explicit subset")
        : scope.sourceDescription;
    result.sourceN = datasetStableRowIds.size();
    const auto idFor = [&](int row) {
        if (row >= 1 && static_cast<std::size_t>(row) <= datasetStableRowIds.size())
            return datasetStableRowIds[static_cast<std::size_t>(row - 1)];
        return version.storageKey + ":missing-row:" + std::to_string(row);
    };
    const std::vector<int> requested = scope.kind == AnalysisScopeKind::AllObservations
        ? ResolveAnalysisScopeRowIds(scope, datasetStableRowIds.size()) : scope.originalRowIds;
    for (int row : requested) result.requestedStableRowIds.push_back(idFor(row));
    const std::vector<int> effective = effectiveOriginalRows.empty() ? requested : effectiveOriginalRows;
    for (int row : effective) result.effectiveStableRowIds.push_back(idFor(row));
    for (int row : excludedOriginalRows) result.excludedStableRowIds.push_back(idFor(row));
    result.scopeN = result.requestedStableRowIds.size();
    result.effectiveN = result.effectiveStableRowIds.size();
    if (scope.invalidatedRowCount) {
        result.warnings.push_back(std::to_string(scope.invalidatedRowCount) +
                                  " scoped row identities are no longer present.");
    }
    return result;
}

std::string ProvenanceRStringLiteral(const std::string &value)
{
    std::ostringstream out;
    out << '"';
    for (unsigned char ch : value) {
        switch (ch) {
        case '\\': out << "\\\\"; break;
        case '"': out << "\\\""; break;
        case '\n': out << "\\n"; break;
        case '\r': out << "\\r"; break;
        case '\t': out << "\\t"; break;
        default: out << static_cast<char>(ch); break;
        }
    }
    out << '"';
    return out.str();
}

std::string RNameLiteral(const std::string &value)
{
    std::string escaped;
    escaped.reserve(value.size());
    for (char ch : value) {
        if (ch == '`' || ch == '\\') escaped.push_back('\\');
        escaped.push_back(ch);
    }
    return "`" + escaped + "`";
}

std::string BuildDataProvenanceRCode(const DataProvenance &provenance)
{
    std::ostringstream out;
    out << "# Data source: " << (provenance.currentVersion.displayName.empty()
        ? provenance.currentVersion.datasetId : provenance.currentVersion.displayName) << "\n"
        << "# Dataset version: " << provenance.currentVersion.version << "\n"
        << "# Object type: " << provenance.currentVersion.objectType << "\n";
    if (provenance.currentVersion.imputationCount > 0)
        out << "# Number of imputations: " << provenance.currentVersion.imputationCount << "\n";
    AppendOriginCode(out, provenance.origin, provenance.originCode,
                     provenance.originDescription);
    if (provenance.currentVersion.objectType == "mids" ||
        provenance.currentVersion.imputationCount > 0) {
        out << "\n# Preserved multiple-imputation object (LinkEDA execution context).\n"
               "# ls_get_data_version() is a stable LinkEDA API; this snapshot keeps the\n"
               "# mids identity and all completed imputations separate from the data-sheet view.\n"
            << "mids_version <- LinkEDA::ls_get_data_version("
            << ProvenanceRStringLiteral(provenance.currentVersion.datasetId)
            << ", version = " << provenance.currentVersion.version << ")\n"
               "stopifnot(inherits(mids_version, \"linkeda_multiple_imputation_version\"))\n"
            << "stopifnot(length(LinkEDA::ls_complete_data_version(mids_version)) == "
            << provenance.currentVersion.imputationCount << "L)\n"
               "if (!is.null(mids_version$mids_object)) {\n"
               "  mids_object <- mids_version$mids_object\n"
            << "  stopifnot(inherits(mids_object, \"mids\"), mids_object$m == "
            << provenance.currentVersion.imputationCount << "L)\n"
               "}\n\n"
               "# Data-sheet view preparation (not provenance of the mids object).\n"
               "displayed_imputation_index <- 1L\n"
               "display_data <- LinkEDA::ls_complete_data_version(mids_version)[[displayed_imputation_index]]\n";
    }
    for (const TransformationStep &step : provenance.history) {
        out << "\n# Transformation: " << step.label << "\n";
        AppendOriginCode(out, step.origin, step.rCode, {});
    }
    return out.str();
}

std::string BuildColumnProvenanceRCode(const DataProvenance &provenance,
                                       const std::string &column)
{
    std::ostringstream out;
    out << "# Column: " << column << "\n"
        << "# Data source: " << provenance.currentVersion.datasetId << "\n"
        << "# Dataset version: " << provenance.currentVersion.version << "\n";
    const auto steps = OrderedColumnSteps(provenance, column);
    if (steps.empty()) {
        out << "# No R transformation is recorded for this column.\n";
        return out.str();
    }
    for (const TransformationStep *step : steps) {
        out << "\n# Transformation: " << step->label << "\n";
        AppendOriginCode(out, step->origin, step->rCode, {});
    }
    return out.str();
}

std::string BuildAnalysisProvenanceRCode(const AnalysisProvenance &provenance,
                                         const std::string &outputBlockId)
{
    std::ostringstream out;
    const DataVersionReference &data = provenance.dataVersion;
    out << "# 1. Data source\n"
        << "# Data source: " << (data.displayName.empty() ? data.datasetId : data.displayName) << "\n"
        << "# Dataset version: " << data.version << "\n"
        << "# Object type: " << data.objectType << "\n";
    if (data.imputationCount > 0) out << "# Number of imputations: " << data.imputationCount << "\n";
    out << "source_data <- LinkEDA::ls_get_data_version("
        << ProvenanceRStringLiteral(data.datasetId) << ", version = " << data.version << ")\n"
        << "# To inspect how this data version was created, use\n"
           "# Export > R Code > Show R Code... on the data source.\n\n"
        << "# 2. Analysis scope\n"
        << "# Analysis scope: " << provenance.scope.description << "\n"
        << "# Analysis scope N: " << provenance.scope.scopeN << " of " << provenance.scope.sourceN << "\n"
        << "# Analyzed N: " << provenance.scope.effectiveN << "\n"
        << "# Analysis-specific exclusions: " << provenance.scope.excludedStableRowIds.size() << "\n";
    const bool multipleImputation = data.imputationCount > 0;
    if (provenance.scope.kind == "all") {
        if (multipleImputation) out << "scoped_source_data <- source_data\n";
        else out << "analysis_data <- source_data\n";
    } else {
        out << "scope_row_ids <- " << RCharacterVector(provenance.scope.requestedStableRowIds) << "\n"
            << (multipleImputation ? "scoped_source_data" : "analysis_data")
            << " <- LinkEDA::ls_select_rows_by_id(source_data, scope_row_ids)\n";
    }
    out << "# Requested N: " << provenance.scope.scopeN << "\n"
        << "# Effective N: " << provenance.scope.effectiveN << "\n";
    if (provenance.scope.kind != "all") {
        out << "effective_row_ids <- "
            << RCharacterVector(provenance.scope.effectiveStableRowIds) << "\n"
            << "excluded_row_ids <- "
            << RCharacterVector(provenance.scope.excludedStableRowIds) << "\n";
    }
    for (const std::string &warning : provenance.scope.warnings)
        out << "# Warning: " << warning << "\n";
    out << "\n# 3. Statistical analysis\n";
    if (provenance.codeOrigin == RCodeOrigin::Unavailable || provenance.executedRCode.empty()) {
        out << "# The R analysis code for this legacy or native-only result is not available.\n";
    } else {
        if (provenance.codeOrigin == RCodeOrigin::Reconstructed)
            out << "# Reconstructed from result metadata; this is not recorded execution code.\n";
        out << provenance.executedRCode;
        if (provenance.executedRCode.back() != '\n') out << '\n';
    }
    if (!outputBlockId.empty()) {
        auto found = provenance.outputRCode.find(outputBlockId);
        if (found != provenance.outputRCode.end() && !found->second.empty()) {
            out << "\n# 4. Result represented by this output\n" << found->second;
            if (!found->second.empty() && found->second.back() != '\n') out << '\n';
        } else {
            out << "\n# 4. Result represented by this output\n"
                   "# No output-specific R object was recorded for this legacy output.\n"
                   "output_result <- NULL\n";
        }
    } else {
        out << "\n# 4. Result represented by this output\n"
               "# Complete analysis result; no narrower table or graph was requested.\n"
               "output_result <- NULL\n";
    }
    out << "\n# The statistical values shown here were computed in R.\n"
           "# The on-screen rendering is handled by the LinkEDA interface.\n";
    return out.str();
}

std::string BuildAnalysisVerificationRCode(const AnalysisProvenance &provenance,
                                           const std::string &outputBlockId)
{
    std::ostringstream out;
    out << "# Reference R code generated to check this LinkEDA result.\n"
           "# This is not a dump of LinkEDA's internal implementation.\n"
        << "# Data source: "
        << (provenance.dataVersion.displayName.empty()
                ? provenance.dataVersion.datasetId
                : provenance.dataVersion.displayName) << "\n"
        << "# Dataset version: " << provenance.dataVersion.version << "\n"
        << "# Analysis scope: " << provenance.scope.description << "\n"
        << "# Analysis scope N: " << provenance.scope.scopeN << " of " << provenance.scope.sourceN << "\n"
        << "# Analyzed N: " << provenance.scope.effectiveN << "\n"
        << "# Analysis-specific exclusions: " << provenance.scope.excludedStableRowIds.size() << "\n";
    for (const std::string &warning : provenance.verificationWarnings)
        out << "# Limitation: " << warning << "\n";
    const auto found = provenance.verificationRCode.find(outputBlockId);
    if (found == provenance.verificationRCode.end() || found->second.empty()) {
        out << "\n# A portable verification recipe is not yet available for this output.\n"
               "# LinkEDA has retained internal provenance for diagnosis, but it is not\n"
               "# exported as user-facing verification code.\n";
        return out.str();
    }
    out << "\n# Verification data have not been exported yet.\n"
           "# Use 'Save Quarto + Data...' to create the verification bundle.\n"
           "verification_data_path <- NULL\n"
           "if (is.null(verification_data_path))\n"
           "  stop(\"Export the verification data from LinkEDA before running this code.\")\n\n"
        << found->second;
    if (found->second.back() != '\n') out << '\n';
    return out.str();
}

bool BindAnalysisVerificationDataPath(const std::string &code,
                                      const std::string &absolutePath,
                                      std::string &boundCode)
{
    boundCode = code;
    if (absolutePath.empty()) return false;
    const std::string prefix = "verification_data_path <- ";
    const std::size_t position = boundCode.find(prefix);
    if (position == std::string::npos) return false;
    const std::size_t lineEnd = boundCode.find('\n', position);
    const std::size_t length = lineEnd == std::string::npos
        ? boundCode.size() - position : lineEnd - position;
    boundCode.replace(position, length,
                      prefix + ProvenanceRStringLiteral(absolutePath));
    const std::string pending = "# Verification data have not been exported yet.";
    const std::size_t pendingPosition = boundCode.find(pending);
    if (pendingPosition != std::string::npos) {
        boundCode.replace(pendingPosition, pending.size(),
                          "# Verification data exported by LinkEDA.");
    }
    return true;
}

std::string BuildQuartoVerificationDocument(const std::string &title,
                                             const std::string &rCode)
{
    // YAML single-quoted strings escape an embedded quote by doubling it.
    std::string yamlTitle = title.empty() ? "LinkEDA verification" : title;
    std::size_t quote = 0;
    while ((quote = yamlTitle.find('\'', quote)) != std::string::npos) {
        yamlTitle.insert(quote, 1, '\'');
        quote += 2;
    }
    std::replace(yamlTitle.begin(), yamlTitle.end(), '\n', ' ');
    std::replace(yamlTitle.begin(), yamlTitle.end(), '\r', ' ');

    std::ostringstream out;
    out << "---\n"
           "title: '" << yamlTitle << "'\n"
           "format:\n"
           "  html: default\n"
           "  pdf:\n"
           "    keep-tex: true\n"
           "  typst: default\n"
           "editor: visual\n"
           "---\n\n"
           "```{r}\n"
           "#| label: linkeda-verification\n"
           "#| echo: false\n"
        << rCode;
    if (!rCode.empty() && rCode.back() != '\n') out << '\n';
    out << "```\n";
    return out.str();
}

namespace {
std::string PublicationClipboardText(const PublicationValue &value,
                                     const PublicationTableColumn &column)
{
    if (value.kind == PublicationValueKind::Missing) return "";
    if (value.kind == PublicationValueKind::Text) return value.text;
    if (value.kind == PublicationValueKind::Logical) return value.logical ? "Yes" : "No";
    if (!std::isfinite(value.number)) return "";
    if (column.pValue && value.number < .001) return "< .001";
    std::ostringstream out;
    out.imbue(std::locale::classic());
    if (column.decimals >= 0) {
        out << std::fixed << std::setprecision(column.decimals) << value.number;
    } else {
        out << std::setprecision(6) << value.number;
    }
    std::string result = out.str();
    if (column.pValue && !result.empty() && result[0] == '0') result.erase(0, 1);
    return result;
}

std::string PublicationHtmlEscape(const std::string &value)
{
    std::string escaped;
    for (char character : value) {
        switch (character) {
        case '&': escaped += "&amp;"; break;
        case '<': escaped += "&lt;"; break;
        case '>': escaped += "&gt;"; break;
        case '"': escaped += "&quot;"; break;
        default: escaped += character;
        }
    }
    return escaped;
}

std::vector<std::size_t> PublicationClipboardColumns(const PublicationTableSpec &spec)
{
    std::vector<std::size_t> visible;
    for (std::size_t j = 0; j < spec.columns.size(); ++j)
        if (j == 0 || PublicationColumnHasContent(spec, j)) visible.push_back(j);
    return visible;
}
} // namespace

std::string BuildPublicationTableTsv(const PublicationTableSpec &spec)
{
    const auto visible = PublicationClipboardColumns(spec);
    std::ostringstream out;
    for (std::size_t i = 0; i < visible.size(); ++i) {
        if (i) out << '\t';
        const auto &column = spec.columns[visible[i]];
        out << (column.label.empty() ? column.key : column.label);
    }
    out << '\n';
    for (const auto &row : spec.rows) {
        for (std::size_t i = 0; i < visible.size(); ++i) {
            if (i) out << '\t';
            const std::size_t j = visible[i];
            if (j < row.size()) out << PublicationClipboardText(row[j], spec.columns[j]);
        }
        out << '\n';
    }
    return out.str();
}

std::string PublicationTitleWithoutTableLabel(const std::string &title)
{
    const auto first = title.find_first_not_of(" \t\r\n");
    if (first == std::string::npos) return {};
    std::string value = title.substr(first);
    if (value.size() < 5 ||
        std::tolower(static_cast<unsigned char>(value[0])) != 't' ||
        std::tolower(static_cast<unsigned char>(value[1])) != 'a' ||
        std::tolower(static_cast<unsigned char>(value[2])) != 'b' ||
        std::tolower(static_cast<unsigned char>(value[3])) != 'l' ||
        std::tolower(static_cast<unsigned char>(value[4])) != 'e') return value;
    std::size_t cursor = 5;
    if (cursor >= value.size() || !std::isspace(static_cast<unsigned char>(value[cursor])))
        return value;
    while (cursor < value.size() && std::isspace(static_cast<unsigned char>(value[cursor]))) ++cursor;
    const std::size_t numberStart = cursor;
    while (cursor < value.size() && std::isdigit(static_cast<unsigned char>(value[cursor]))) ++cursor;
    if (cursor == numberStart) return value;
    while (cursor < value.size() && std::isspace(static_cast<unsigned char>(value[cursor]))) ++cursor;
    if (cursor < value.size() && (value[cursor] == '.' || value[cursor] == ':' || value[cursor] == '-')) ++cursor;
    while (cursor < value.size() && std::isspace(static_cast<unsigned char>(value[cursor]))) ++cursor;
    return cursor < value.size() ? value.substr(cursor) : std::string();
}

std::string BuildPublicationTableHtml(const PublicationTableSpec &spec,
                                      bool apa7,
                                      const std::string &tableNumber,
                                      const std::string &tableTitle)
{
    const auto visible = PublicationClipboardColumns(spec);
    std::ostringstream out;
    out << "<html><head><meta charset=\"utf-8\"></head><body>";
    if (apa7) {
        const std::string apaTitle = PublicationTitleWithoutTableLabel(
            tableTitle.empty() ? spec.title : tableTitle);
        out << "<p style=\"margin:0;font-weight:bold\">Table "
            << PublicationHtmlEscape(tableNumber.empty() ? "1" : tableNumber) << "</p>";
        out << "<p style=\"margin:0 0 8px;font-style:italic\">"
            << PublicationHtmlEscape(apaTitle) << "</p>";
    } else if (!spec.title.empty()) {
        out << "<p style=\"font-weight:bold\">" << PublicationHtmlEscape(spec.title) << "</p>";
    }
    out << "<table cellspacing=\"0\" cellpadding=\"4\" style=\"border-collapse:collapse;"
           "font-family:Arial,sans-serif;font-size:10pt;width:100%\"><thead><tr>";
    for (std::size_t j : visible) {
        const auto &column = spec.columns[j];
        out << "<th style=\"border-top:1px solid black;border-bottom:1px solid black;"
               "text-align:" << (column.pValue || column.decimals >= 0 ? "right" : "left")
            << "\">" << PublicationHtmlEscape(column.label.empty() ? column.key : column.label)
            << "</th>";
    }
    out << "</tr></thead><tbody>";
    for (std::size_t i = 0; i < spec.rows.size(); ++i) {
        const auto &row = spec.rows[i];
        out << "<tr>";
        for (std::size_t j : visible) {
            const auto &column = spec.columns[j];
            std::string value = j < row.size() ? PublicationClipboardText(row[j], column) : "";
            std::size_t indent = j == 0 ? value.find_first_not_of(' ') : 0;
            if (indent == std::string::npos) indent = 0;
            out << "<td style=\"border-bottom:"
                << (i + 1 == spec.rows.size() ? "1px solid black" : "0")
                << ";text-align:" << (column.pValue || column.decimals >= 0 ? "right" : "left")
                << (indent ? ";padding-left:18px" : "") << "\">"
                << PublicationHtmlEscape(value.substr(indent)) << "</td>";
        }
        out << "</tr>";
    }
    out << "</tbody></table>";
    if (!spec.subtitle.empty()) out << "<p>" << PublicationHtmlEscape(spec.subtitle) << "</p>";
    if (!spec.footnotes.empty()) {
        out << "<p><i>Note.</i> ";
        for (std::size_t i = 0; i < spec.footnotes.size(); ++i) {
            if (i) out << " ";
            out << PublicationHtmlEscape(spec.footnotes[i]);
        }
        out << "</p>";
    }
    out << "</body></html>";
    return out.str();
}

static std::string TinytablePublicationRCode(const PublicationTableSpec &spec, bool apa7, bool displayed = false)
{
    // Publication tables are a semantic view, not a verbatim dump of the
    // native result grid.  Empty statistical columns are omitted and the
    // designated stub is given enough room to retain the term hierarchy.
    std::vector<std::size_t> visibleColumns;
    for (std::size_t column = 0; column < spec.columns.size(); ++column) {
        if (displayed || column == 0 || PublicationColumnHasContent(spec, column))
            visibleColumns.push_back(column);
    }
    std::size_t stubColumn = 0;
    if (!spec.stubColumns.empty()) stubColumn = spec.stubColumns.front();
    auto visibleStub = std::find(visibleColumns.begin(), visibleColumns.end(), stubColumn);
    const std::size_t stubPosition = visibleStub == visibleColumns.end()
        ? 1 : static_cast<std::size_t>(visibleStub - visibleColumns.begin()) + 1;

    std::ostringstream out;
    out << "# Publication/reproduction code generated by LinkEDA.\n"
           "# This formats the statistical results already computed by LinkEDA.\n"
           "if (!requireNamespace(\"tinytable\", quietly = TRUE))\n"
           "  stop(\"Install package 'tinytable' to format this table.\")\n"
           "result_data <- data.frame(\n";
    for (std::size_t visible = 0; visible < visibleColumns.size(); ++visible) {
        const std::size_t column = visibleColumns[visible];
        if (visible) out << ",\n";
        out << "  " << RNameLiteral(spec.columns[column].key) << " = c(";
        for (std::size_t row = 0; row < spec.rows.size(); ++row) {
            if (row) out << ", ";
            out << (column < spec.rows[row].size() ? RValue(spec.rows[row][column]) : "NA");
        }
        out << ")";
    }
    out << ",\n  check.names = FALSE\n)\n"
           "names(result_data) <- "
        << RCharacterVector([&] {
               std::vector<std::string> labels;
               for (std::size_t column : visibleColumns) {
                   const PublicationTableColumn &definition = spec.columns[column];
                   labels.push_back(definition.label.empty()
                       ? definition.key : definition.label);
               }
               return labels;
           }()) << "\n";
    out << "publication_indent_rows <- grep(\"^[[:space:]]+\", result_data[["
        << stubPosition << "L]])\n"
           "result_data[[" << stubPosition << "L]] <- trimws(result_data[["
        << stubPosition << "L]])\n";
    const std::string caption = spec.subtitle.empty() ? spec.title :
        (spec.title.empty() ? spec.subtitle : spec.title + " — " + spec.subtitle);
    std::string alignment;
    std::vector<int> widths;
    for (std::size_t visible = 0; visible < visibleColumns.size(); ++visible) {
        const PublicationTableColumn &column = spec.columns[visibleColumns[visible]];
        alignment.push_back(column.pValue || column.decimals >= 0 ? 'r' : 'l');
        const bool isStub = std::find(spec.stubColumns.begin(), spec.stubColumns.end(),
            visibleColumns[visible]) != spec.stubColumns.end();
        widths.push_back(spec.stubColumns.size() > 1 && isStub ? 2 :
            (visible + 1 == stubPosition && (!displayed || !spec.stubColumns.empty()) ? (displayed ? 4 : 3) :
            (column.label.find("CI") != std::string::npos ? (displayed ? 3 : (apa7 ? 2 : 1)) : 1)));
    }
    out << "publication_table <- tinytable::tt(\n"
           "  result_data, caption = " << (apa7 ? "NULL" : ProvenanceRStringLiteral(caption))
        << ", notes = " << RStringList(spec.footnotes)
        << ", theme = " << ProvenanceRStringLiteral(apa7 || displayed ? "empty" : "striped") << ", escape = TRUE, align = "
        << ProvenanceRStringLiteral(alignment) << ", width = c(";
    for (std::size_t index = 0; index < widths.size(); ++index) {
        if (index) out << ", ";
        out << widths[index];
    }
    out << ")\n)\n"
           "publication_table <- tinytable::format_tt(publication_table, replace = TRUE)\n"
           "publication_table <- tinytable::style_tt(publication_table, i = 0, bold = TRUE)\n"
           "if (length(publication_indent_rows))\n"
           "  publication_table <- tinytable::style_tt(publication_table, i = publication_indent_rows, j = "
        << stubPosition << ", indent = 1)\n";
    for (std::size_t visible = 0; visible < visibleColumns.size(); ++visible) {
        const PublicationTableColumn &column = spec.columns[visibleColumns[visible]];
        const std::size_t rColumn = visible + 1;
        out << "publication_table <- tinytable::style_tt(publication_table, j = "
            << rColumn << ", align = \"" << alignment[visible] << "\")\n";
        if (column.pValue) {
            out << "publication_table <- tinytable::format_tt(publication_table, j = "
                << rColumn
                << ", fn = function(x) ifelse(is.na(x), \"\", ifelse(x < .001, \"< .001\", sub(\"^0\", \"\", sprintf(\"%.3f\", x)))))\n";
        } else if (column.decimals >= 0) {
            if (apa7) out << "publication_table <- tinytable::format_tt(publication_table, j = "
                << rColumn << ", fn = function(x) ifelse(is.na(x), '', sprintf('%."
                << column.decimals << "f', x)))\n";
            else {
            out << "publication_table <- tinytable::format_tt(publication_table, j = "
                << rColumn << ", digits = "
                << column.decimals << ", num_fmt = \"decimal\", replace = TRUE)\n";
            }
        }
    }
    if (displayed) {
        out << "publication_table <- tinytable::style_tt(publication_table, i = 0, line = 'b', line_color = '#AAAAAA', line_width = .04)\n";
    }
    if (apa7) {
        out << "publication_table <- tinytable::style_tt(publication_table, i = 0, bold = FALSE, line = 'tb', line_width = .04)\n"
               "publication_table <- tinytable::style_tt(publication_table, i = nrow(result_data), line = 'b', line_width = .04)\n"
               "publication_parent_rows <- setdiff(seq_len(nrow(result_data)), publication_indent_rows)\n"
               "if (length(publication_indent_rows) && length(publication_parent_rows))\n"
               "  publication_table <- tinytable::style_tt(publication_table, i = publication_parent_rows, j = 1, bold = TRUE)\n"
               "publication_symbols <- which(names(result_data) %in% c('N','M','SD','SE','t','F','p','df','d','g','r'))\n"
               "if (length(publication_symbols))\n"
               "  publication_table <- tinytable::style_tt(publication_table, i = 0, j = publication_symbols, italic = TRUE)\n";
    }
    // A sourced script does not auto-print its final expression in RStudio.
    // Explicit printing also dispatches tinytable's rich renderer in notebooks.
    out << "print(publication_table)\n";
    return out.str();
}

std::string BuildTinytablePublicationRCode(const PublicationTableSpec &spec)
{
    return TinytablePublicationRCode(spec, false);
}

std::string Ggplot2ThemeRExpression(const std::string &theme)
{
    if (theme == "classic") return "ggplot2::theme_classic()";
    if (theme == "minimal") return "ggplot2::theme_minimal()";
    if (theme == "gray") return "ggplot2::theme_gray()";
    if (theme == "bw") return "ggplot2::theme_bw()";
    if (theme == "publication")
        return "ggplot2::theme_classic(base_size = 11) + ggplot2::theme("
               "panel.grid.major = ggplot2::element_line(colour = \"#EDEDED\", linewidth = 0.35), "
               "panel.grid.minor = ggplot2::element_blank(), "
               "axis.line = ggplot2::element_line(colour = \"#333333\", linewidth = 0.5), "
               "plot.title = ggplot2::element_text(face = \"bold\", size = 12), "
               "plot.subtitle = ggplot2::element_text(colour = \"#666666\"))";
    if (theme == "cowplot") {
        return "if (requireNamespace(\"cowplot\", quietly = TRUE)) "
               "cowplot::theme_cowplot() else ggplot2::theme_bw()";
    }
    if (theme == "ipsum") {
        return "if (requireNamespace(\"hrbrthemes\", quietly = TRUE)) "
               "hrbrthemes::theme_ipsum() else ggplot2::theme_bw()";
    }
    if (theme == "theme_tq") {
        return "if (requireNamespace(\"tidyquant\", quietly = TRUE)) "
               "tidyquant::theme_tq() else ggplot2::theme_bw()";
    }
    if (theme == "tufte") {
        return "if (requireNamespace(\"ggthemes\", quietly = TRUE)) "
               "ggthemes::theme_tufte() else ggplot2::theme_bw()";
    }
    if (theme == "economist") {
        return "if (requireNamespace(\"ggthemes\", quietly = TRUE)) "
               "ggthemes::theme_economist() else ggplot2::theme_bw()";
    }
    if (theme == "fivethirtyeight") {
        return "if (requireNamespace(\"ggthemes\", quietly = TRUE)) "
               "ggthemes::theme_fivethirtyeight() else ggplot2::theme_bw()";
    }
    // theme_modern and LinkEDA's historical MANET, ViSta, Beige, DataDesk,
    // and Garish themes have no reliable public ggplot2 equivalent.  Do not
    // silently substitute another named design.
    return "ggplot2::theme_bw()";
}

std::string BuildGgplot2PublicationRCode(const PublicationPlotSpec &spec)
{
    std::vector<std::string> series, labels;
    std::vector<double> xs, ys, lowers, uppers;
    for (const PublicationPlotSeries &entry : spec.series) {
        const std::size_t n = std::min(entry.x.size(), entry.y.size());
        for (std::size_t i = 0; i < n; ++i) {
            series.push_back(entry.id); labels.push_back(entry.label);
            xs.push_back(entry.x[i]); ys.push_back(entry.y[i]);
            lowers.push_back(i < entry.lower.size() ? entry.lower[i] : NAN);
            uppers.push_back(i < entry.upper.size() ? entry.upper[i] : NAN);
        }
    }
    auto numbers = [](const std::vector<double> &values) {
        std::ostringstream result; result << "c(";
        for (std::size_t i = 0; i < values.size(); ++i) {
            if (i) result << ", "; result << RNumber(values[i]);
        }
        result << ")"; return result.str();
    };
    std::ostringstream out;
    out << "# Publication/reproduction code generated by LinkEDA.\n"
           "# The statistical values were computed by the analysis associated with this output.\n"
           "if (!requireNamespace(\"ggplot2\", quietly = TRUE))\n"
           "  stop(\"Install package 'ggplot2' to render this graph.\")\n"
        << "linkeda_plot_theme <- " << Ggplot2ThemeRExpression(spec.theme) << "\n"
        <<
           "plot_data <- data.frame(\n"
        << "  series_id = " << RCharacterVector(series) << ",\n"
        << "  series = " << RCharacterVector(labels) << ",\n"
        << "  x = " << numbers(xs) << ",\n"
        << "  y = " << numbers(ys) << ",\n"
        << "  conf.low = " << numbers(lowers) << ",\n"
        << "  conf.high = " << numbers(uppers) << ",\n"
           "  check.names = FALSE\n)\n"
        << "plot_data$series <- factor(plot_data$series, levels = "
        << RCharacterVector([&] { std::vector<std::string> order; for (const auto &s : spec.series) order.push_back(s.label); return order; }()) << ")\n";
    if (!spec.xCategoryOrder.empty()) {
        out << "x_positions <- sort(unique(plot_data$x))\n"
               "if (length(x_positions) != " << spec.xCategoryOrder.size() << "L) {\n"
               "  stop('Stored category labels do not match the plotted x positions.')\n"
               "}\n"
               "plot_data$x_display <- factor(\n"
               "  match(plot_data$x, x_positions),\n"
               "  levels = seq_along(x_positions),\n"
               "  labels = " << RCharacterVector(spec.xCategoryOrder) << "\n"
               ")\n";
    }

    const bool histogram = spec.kind == "histogram" ||
        spec.kind == "histogram_density";
    const bool barplot = spec.kind == "barplot";
    const bool boxplot = spec.kind == "boxplot" ||
        spec.kind == "violin" || spec.kind == "boxplot_violin";
    const bool scatter = spec.kind == "scatter" ||
        spec.kind == "trellis_scatterplot";
    const bool timeSeries = spec.kind == "time_series";
    const bool multipleSeries = spec.series.size() > 1;

    if (histogram) {
        out << "histogram_data <- plot_data[plot_data$series_id == \"histogram\", , drop = FALSE]\n"
               "publication_plot <- ggplot2::ggplot(histogram_data) +\n"
               "  ggplot2::geom_rect(\n"
               "    ggplot2::aes(xmin = conf.low, xmax = conf.high, ymin = 0, ymax = y),\n"
               "    fill = \"grey75\", colour = \"white\"\n"
               "  )\n";
        if (spec.kind == "histogram_density") {
            out << "density_source <- plot_data[plot_data$series_id == \"density_source\", , drop = FALSE]\n"
                   "bin_width <- stats::median(histogram_data$conf.high - histogram_data$conf.low, na.rm = TRUE)\n"
                   "if (nrow(density_source) > 1L && is.finite(bin_width) && bin_width > 0) {\n"
                   "  publication_plot <- publication_plot + ggplot2::geom_density(\n"
                   "    data = density_source,\n"
                   "    ggplot2::aes(x = x, y = ggplot2::after_stat(count * bin_width)),\n"
                   "    inherit.aes = FALSE, linewidth = 0.8, colour = \"#0072B2\", fill = NA\n"
                   "  )\n"
                   "}\n";
        }
    } else if (barplot) {
        out << "publication_plot <- ggplot2::ggplot(\n"
               "  plot_data, ggplot2::aes(x = x_display, y = y, fill = series)\n"
               ") + ggplot2::geom_col(position = \"stack\", colour = \"white\", linewidth = 0.25)\n";
        if (!multipleSeries)
            out << "publication_plot <- publication_plot + ggplot2::guides(fill = \"none\")\n";
    } else if (boxplot) {
        out << "publication_plot <- ggplot2::ggplot(\n"
               "  plot_data, ggplot2::aes(x = x_display, y = y)\n"
               ")\n";
        if (spec.kind == "violin" || spec.kind == "boxplot_violin")
            out << "publication_plot <- publication_plot + ggplot2::geom_violin(\n"
                   "  trim = FALSE, fill = \"grey85\", colour = \"grey35\", na.rm = TRUE\n"
                   ")\n";
        if (spec.kind == "boxplot" || spec.kind == "boxplot_violin")
            out << "publication_plot <- publication_plot + ggplot2::geom_boxplot(\n"
                   "  width = 0.35, outlier.shape = NA, fill = \"white\", na.rm = TRUE\n"
                   ")\n";
        if (spec.showLines)
            out << "publication_plot <- publication_plot + ggplot2::geom_line(\n"
                   "  ggplot2::aes(group = series_id), alpha = 0.35, linewidth = 0.5, na.rm = TRUE\n"
                   ")\n";
        if (spec.showPoints)
            out << "publication_plot <- publication_plot + ggplot2::geom_jitter(\n"
                   "  width = 0.08, height = 0, alpha = 0.65, size = 1.7, na.rm = TRUE\n"
                   ")\n";
    } else if (scatter || timeSeries) {
        if (multipleSeries) {
            out << "publication_plot <- ggplot2::ggplot(\n"
                   "  plot_data, ggplot2::aes(x = x, y = y, colour = series, group = series)\n"
                   ")\n";
        } else {
            out << "publication_plot <- ggplot2::ggplot(\n"
                   "  plot_data, ggplot2::aes(x = x, y = y)\n"
                   ")\n";
        }
        if (timeSeries || spec.showLines)
            out << "publication_plot <- publication_plot + ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE)\n";
        if (spec.showPoints)
            out << "publication_plot <- publication_plot + ggplot2::geom_point(size = 2.0, na.rm = TRUE)\n";
    } else {
        if (!spec.xCategoryOrder.empty()) {
            out << "publication_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x_display, y = y, colour = series, group = series))\n";
        } else {
            out << "publication_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y, colour = series, group = series))\n";
        }
    }

    const bool categoricalAxis = !spec.xCategoryOrder.empty();
    if (!histogram && !barplot && !boxplot && !scatter && !timeSeries &&
        categoricalAxis)
        out << "category_dodge <- ggplot2::position_dodge(width = 0.35)\n";
    if (!histogram && !barplot && !boxplot && !scatter && !timeSeries &&
        spec.showConfidenceIntervals) {
        if (categoricalAxis)
            out << "publication_plot <- publication_plot + ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high), width = 0.10, position = category_dodge)\n";
        else
            out << "publication_plot <- publication_plot + ggplot2::geom_ribbon(ggplot2::aes(ymin = conf.low, ymax = conf.high, fill = series), alpha = 0.16, colour = NA)\n";
    }
    if (!histogram && !barplot && !boxplot && !scatter && !timeSeries &&
        spec.showLines)
        out << "publication_plot <- publication_plot + ggplot2::geom_line(linewidth = 0.8"
            << (categoricalAxis ? ", position = category_dodge" : "") << ")\n";
    if (!histogram && !barplot && !boxplot && !scatter && !timeSeries &&
        spec.showPoints)
        out << "publication_plot <- publication_plot + ggplot2::geom_point(size = 2.2"
            << (categoricalAxis ? ", position = category_dodge" : "") << ")\n";
    const std::string legendTitle = spec.legendTitleVisible && !spec.legendTitle.empty()
        ? ProvenanceRStringLiteral(spec.legendTitle) : "NULL";
    out << "publication_plot <- publication_plot + ggplot2::labs(title = " << ProvenanceRStringLiteral(spec.title)
        << ", subtitle = " << ProvenanceRStringLiteral(spec.subtitle)
        << ", x = " << ProvenanceRStringLiteral(spec.xLabel) << ", y = " << ProvenanceRStringLiteral(spec.yLabel)
        << ", colour = " << legendTitle << ", fill = " << legendTitle
        << ") + linkeda_plot_theme\n";
    if (!spec.showAxisTickMarks || !spec.showAxisTickLabels) {
        out << "publication_plot <- publication_plot + ggplot2::theme(\n";
        if (!spec.showAxisTickMarks)
            out << "  axis.ticks = ggplot2::element_blank()";
        if (!spec.showAxisTickMarks && !spec.showAxisTickLabels)
            out << ",\n";
        if (!spec.showAxisTickLabels)
            out << "  axis.text = ggplot2::element_blank()";
        out << "\n)\n";
    }
    out << "publication_plot\n";
    return out.str();
}

static std::string PublicationLatexText(const std::string &text)
{
    std::string escaped;
    for (char c : text) {
        switch(c) {
        case '\\': escaped += "\\textbackslash{}"; break;
        case '{': case '}': case '$': case '&': case '#': case '%': case '_':
            escaped += '\\'; escaped += c; break;
        case '~': escaped += "\\textasciitilde{}"; break;
        case '^': escaped += "\\textasciicircum{}"; break;
        default: escaped += c;
        }
    }
    return escaped;
}

std::string BuildLatexPublicationRCode(const PublicationTableSpec &spec, bool compilePdf)
{
    return BuildLatexPublicationRCode(std::vector<PublicationTableSpec>{spec}, compilePdf, {});
}

std::string BuildLatexPublicationRCode(const std::vector<PublicationTableSpec> &tables,
    bool compilePdf, const LatexPublicationOptions &options)
{
    bool unicode = options.apa7;
    std::size_t maxColumns = 0;
    std::ostringstream out;
    out << "latex_sections <- character()\n";
    if (options.apa7) {
        const std::string cleanTitle = PublicationTitleWithoutTableLabel(options.title);
        const std::string heading = "\\noindent\\textbf{Table " + PublicationLatexText(options.tableNumber) +
            "}\\par\\vspace{6pt}\n\\noindent\\textit{" + PublicationLatexText(cleanTitle) + "}\\par\\vspace{12pt}";
        out << "latex_sections <- " << ProvenanceRStringLiteral(heading) << "\n";
    }
    for (const auto &spec : tables) {
        maxColumns = std::max(maxColumns, spec.columns.size());
        auto hasUnicode = [](const std::string &text) {
            return std::any_of(text.begin(), text.end(), [](unsigned char c) { return c >= 0x80; });
        };
        unicode = unicode || hasUnicode(spec.title) || hasUnicode(spec.subtitle);
        for (const auto &column : spec.columns) unicode = unicode || hasUnicode(column.label);
        for (const auto &row : spec.rows) for (const auto &value : row)
            unicode = unicode || hasUnicode(value.text);
        for (const auto &note : spec.footnotes) unicode = unicode || hasUnicode(note);
        PublicationTableSpec latexSpec = spec;
        std::string notes;
        for (const auto &note : spec.footnotes) {
            if (note.empty()) continue;
            if (!notes.empty()) {
                if (notes.back() != '.' && notes.back() != '!' && notes.back() != '?') notes += ".";
                notes += " ";
            }
            notes += note;
        }
        if (options.apa7 || options.reportHeadings) {
            latexSpec.title.clear(); latexSpec.subtitle.clear(); latexSpec.footnotes.clear();
            if (!spec.title.empty()) out << "latex_sections <- c(latex_sections, " << ProvenanceRStringLiteral(
                "\\noindent\\textbf{" + PublicationLatexText(spec.title) + "}\\par\\vspace{4pt}") << ")\n";
        } else if (spec.footnotes.size() > 1) latexSpec.footnotes = {notes};
        if (options.reportHeadings && !spec.subtitle.empty())
            out << "latex_sections <- c(latex_sections, " << ProvenanceRStringLiteral(
                "\\noindent " + PublicationLatexText(spec.subtitle) + "\\par\\vspace{6pt}") << ")\n";
        out << TinytablePublicationRCode(latexSpec, options.apa7, options.reportHeadings && !options.apa7) << "\n";
        if (options.reportHeadings && !options.apa7)
            out << "publication_table <- tinytable::theme_latex(publication_table, environment = 'tblr', environment_table = FALSE, inner = 'rowsep=3pt')\n";
        else if (options.apa7) out << "publication_table <- tinytable::theme_latex(publication_table, multipage = TRUE, rowhead = 1)\n";
        out << "latex_table <- tinytable::save_tt(publication_table, output = \"latex\")\n"
               "latex_table <- sub('\\\\begin{table}', '\\\\begin{table}[!htbp]', latex_table, fixed = TRUE)\n"
               "latex_sections <- c(latex_sections, latex_table)\n";
        if (options.reportHeadings && !options.apa7) out << "latex_sections <- c(latex_sections, \"\\\\par\\\\vspace{16pt}\")\n";
        if ((options.apa7 || options.reportHeadings) && !notes.empty()) out << "latex_sections <- c(latex_sections, " << ProvenanceRStringLiteral(
            "\\noindent\\textit{Note.} " + PublicationLatexText(notes) + "\\par\\vspace{12pt}") << ")\n";
    }
    out << "latex_table <- paste(latex_sections, collapse = '\\n')\n"
           "latex_document <- paste(\n"
        << "  " << ProvenanceRStringLiteral(options.reportHeadings && !options.apa7
            ? "\\documentclass[11pt,border=14pt]{standalone}"
            : "\\documentclass[11pt]{article}") << ",\n"
        <<
           "  '\\\\usepackage{tabularray}',\n"
           "  '\\\\usepackage{float}',\n"
           "  '\\\\usepackage{xcolor}',\n"
           "  '\\\\usepackage{graphicx}',\n"
           "  '\\\\usepackage{rotating}',\n"
           "  '\\\\usepackage[normalem]{ulem}',\n"
           "  '\\\\UseTblrLibrary{booktabs,siunitx}',\n"
           "  '\\\\newcommand{\\\\tinytableTabularrayUnderline}[1]{\\\\underline{#1}}',\n"
           "  '\\\\newcommand{\\\\tinytableTabularrayStrikeout}[1]{\\\\sout{#1}}',\n"
           "  '\\\\NewTableCommand{\\\\tinytableDefineColor}[3]{\\\\definecolor{#1}{#2}{#3}}',\n";
    if (!options.reportHeadings || options.apa7) out << "  '\\\\usepackage[a4paper,margin=" << (options.apa7 ? "25.4mm" : "18mm")
        << (options.orientation == "Landscape" || (options.orientation == "Automatic" && maxColumns >= 7) ? ",landscape" : "") << "]{geometry}',\n";
    // Paint an opaque page: unpainted PDF backgrounds disappear on dark paste targets.
    out << "  '\\\\pagecolor{white}',\n"
           "  '\\\\color{black}',\n"
           "  '\\\\pagestyle{empty}',\n";
    // XeTeX may omit a white pagecolor. Paint a real white rectangle at shipout.
    // standalone moves the page origin by one inch while cropping the report.
    const std::string background = "\\AddToHook{shipout/background}{\\put(" +
        std::string(options.reportHeadings && !options.apa7 ? "72bp,-72bp" : "0,0") +
        "){\\color{white}\\rule[-\\paperheight]{\\paperwidth}{\\paperheight}}}";
    out << "  " << ProvenanceRStringLiteral(background) << ",\n";
    if (options.apa7 || options.reportHeadings) out << "  '\\\\DefTblrTemplate{caption}{default}{}',\n"
                            "  '\\\\DefTblrTemplate{capcont}{default}{}',\n";

    if (unicode) out << "  '\\\\usepackage{fontspec}',\n"
        "  '\\\\IfFontExistsTF{Arial}{\\\\setmainfont{Arial}}{}',\n";
    out <<
           "  '\\\\begin{document}',\n";
    if (options.reportHeadings && !options.apa7)
        out << "  '\\\\begin{minipage}{900pt}\\\\setlength{\\\\parindent}{0pt}',\n";
    out << "  latex_table,\n";
    if (options.reportHeadings && !options.apa7) out << "  '\\\\end{minipage}',\n";
    out << "  '\\\\end{document}',\n"
           "  sep = '\\n'\n"
           ")\n"
           "writeLines(latex_document, 'linkeda-table.tex', useBytes = TRUE)\n";
    if (compilePdf) {
        out << "latexmk <- unname(Sys.which('latexmk'))\n"
               "latex_engines <- Sys.which(c('xelatex', 'lualatex'))\n"
               "latex_engine <- unname(latex_engines[latex_engines != ''][1L])\n"
               "if (is.na(latex_engine) || !nzchar(latex_engine)) {\n"
               "  stop('PDF was not produced: XeLaTeX or LuaLaTeX is required.')\n"
               "}\n"
               "status <- if (!is.na(latexmk) && nzchar(latexmk)) {\n"
               "  engine_flag <- if (grepl('xelatex$', latex_engine)) '-xelatex' else '-lualatex'\n"
               "  system2(latexmk, c(engine_flag, '-interaction=nonstopmode', '-halt-on-error', 'linkeda-table.tex'))\n"
               "} else {\n"
               "  system2(latex_engine, c('-interaction=nonstopmode', '-halt-on-error', 'linkeda-table.tex'))\n"
               "}\n"
               "if (!identical(status, 0L) || !file.exists('linkeda-table.pdf')) {\n"
               "  stop('PDF was not produced because LaTeX compilation failed.')\n"
               "}\n";
    }
    return out.str();
}

std::string BuildPublicationRCode(const OutputCodeReference &output,
                                  PublicationBackend backend)
{
    if (backend == PublicationBackend::Ggplot2) {
        if (output.publication.plot)
            return BuildGgplot2PublicationRCode(*output.publication.plot);
        // Some derived plots (notably model diagnostics) intentionally keep
        // an executable R recipe instead of serializing already-computed
        // coordinates.  Reuse that recipe for publication so ggplot2
        // recalculates the diagnostic from the exported prepared data.
        const auto recipe = output.provenance.verificationRCode.find(
            output.outputBlockId);
        if (recipe != output.provenance.verificationRCode.end())
            return recipe->second;
    }
    if (backend == PublicationBackend::Tinytable && output.publication.table)
        return BuildTinytablePublicationRCode(*output.publication.table);
    if ((backend == PublicationBackend::Latex || backend == PublicationBackend::LatexPdf) &&
        output.publication.table)
        return BuildLatexPublicationRCode(*output.publication.table,
                                         backend == PublicationBackend::LatexPdf);
    return {};
}

bool ReadAnalysisProvenancePayload(const std::vector<std::string> &fields,
                                   std::size_t &cursor,
                                   AnalysisProvenance &provenance,
                                   std::string *error)
{
    if (cursor >= fields.size() ||
        (fields[cursor] != "ANALYSIS_PROVENANCE_V1" &&
         fields[cursor] != "ANALYSIS_PROVENANCE_V2"))
        return true;
    const bool verificationPayload = fields[cursor] == "ANALYSIS_PROVENANCE_V2";
    auto fail = [&](const std::string &message) {
        if (error) *error = message;
        return false;
    };
    auto decodeHex = [&](const std::string &encoded, std::string &decoded) {
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
    };
    ++cursor;
    if (cursor + 9U > fields.size())
        return fail("The R analysis provenance trailer is incomplete.");
    provenance.analysisId = fields[cursor++];
    provenance.title = fields[cursor++];
    provenance.dataVersion.datasetId = fields[cursor++];
    provenance.dataVersion.displayName = provenance.dataVersion.datasetId;
    char *versionEnd = nullptr;
    const unsigned long long version = std::strtoull(fields[cursor].c_str(), &versionEnd, 10);
    if (!versionEnd || versionEnd == fields[cursor].c_str() || *versionEnd != '\0' || version == 0)
        return fail("The R analysis provenance data version is invalid.");
    provenance.dataVersion.version = static_cast<std::uint64_t>(version);
    ++cursor;
    provenance.dataVersion.objectType = fields[cursor++];
    char *countEnd = nullptr;
    const long imputationCount = std::strtol(fields[cursor].c_str(), &countEnd, 10);
    if (!countEnd || countEnd == fields[cursor].c_str() || *countEnd != '\0' || imputationCount < 0)
        return fail("The R analysis provenance imputation count is invalid.");
    provenance.dataVersion.imputationCount = static_cast<int>(imputationCount);
    provenance.dataVersion.storageKey = provenance.dataVersion.datasetId + "@" +
        std::to_string(provenance.dataVersion.version);
    ++cursor;
    provenance.codeOrigin = RCodeOriginFromId(fields[cursor++]);
    if (!decodeHex(fields[cursor++], provenance.executedRCode))
        return fail("The R analysis provenance code is not valid UTF-8 hex data.");
    char *outputsEnd = nullptr;
    const long outputCount = std::strtol(fields[cursor].c_str(), &outputsEnd, 10);
    if (!outputsEnd || outputsEnd == fields[cursor].c_str() || *outputsEnd != '\0' ||
        outputCount < 0 || cursor + 1U + static_cast<std::size_t>(2L * outputCount) > fields.size())
        return fail("The R analysis provenance output count is invalid.");
    ++cursor;
    provenance.outputRCode.clear();
    for (long index = 0; index < outputCount; ++index) {
        const std::string id = fields[cursor++];
        std::string code;
        if (id.empty() || !decodeHex(fields[cursor++], code))
            return fail("An R analysis provenance output block is invalid.");
        provenance.outputRCode[id] = std::move(code);
    }
    if (verificationPayload) {
        auto readCount = [&](long &count, const char *label) {
            if (cursor >= fields.size()) return fail(std::string("The ") + label + " count is missing.");
            char *end = nullptr;
            count = std::strtol(fields[cursor].c_str(), &end, 10);
            if (!end || end == fields[cursor].c_str() || *end != '\0' || count < 0)
                return fail(std::string("The ") + label + " count is invalid.");
            ++cursor;
            return true;
        };
        long variableCount = 0;
        if (!readCount(variableCount, "verification variable") ||
            cursor + static_cast<std::size_t>(variableCount) > fields.size())
            return fail("The verification variable payload is incomplete.");
        provenance.verificationVariables.assign(
            fields.begin() + static_cast<std::ptrdiff_t>(cursor),
            fields.begin() + static_cast<std::ptrdiff_t>(cursor + variableCount));
        cursor += static_cast<std::size_t>(variableCount);
        long recipeCount = 0;
        if (!readCount(recipeCount, "verification recipe") ||
            cursor + static_cast<std::size_t>(2L * recipeCount) > fields.size())
            return fail("The verification recipe payload is incomplete.");
        provenance.verificationRCode.clear();
        for (long index = 0; index < recipeCount; ++index) {
            const std::string id = fields[cursor++];
            std::string code;
            if (id.empty() || !decodeHex(fields[cursor++], code))
                return fail("An R verification recipe is invalid.");
            provenance.verificationRCode[id] = std::move(code);
        }
        long warningCount = 0;
        if (!readCount(warningCount, "verification warning") ||
            cursor + static_cast<std::size_t>(warningCount) > fields.size())
            return fail("The verification warning payload is incomplete.");
        provenance.verificationWarnings.assign(
            fields.begin() + static_cast<std::ptrdiff_t>(cursor),
            fields.begin() + static_cast<std::ptrdiff_t>(cursor + warningCount));
        cursor += static_cast<std::size_t>(warningCount);
    }
    if (cursor < fields.size() && fields[cursor] == "MI_DIAGNOSTICS_V1") {
        ++cursor;
        if (cursor >= fields.size()) return fail("Missing MI diagnostics input.");
        provenance.preparedDataPath = fields[cursor++];
        auto count = [&](std::size_t &value) {
            if (cursor >= fields.size()) return false;
            char *end = nullptr;
            const auto parsed = std::strtol(fields[cursor++].c_str(), &end, 10);
            if (!end || *end || parsed < 0 || parsed > 1000000) return false;
            value = static_cast<std::size_t>(parsed); return true;
        };
        std::size_t columns=0, rows=0;
        if (!count(columns) || cursor + columns > fields.size()) return fail("Invalid MI diagnostics columns.");
        provenance.missingInformationColumns.assign(fields.begin()+cursor, fields.begin()+cursor+columns);
        cursor += columns;
        if (!count(rows) || (rows && !columns) || rows > (fields.size()-cursor)/std::max<std::size_t>(1,columns)) return fail("Invalid MI diagnostics rows.");
        provenance.missingInformationRows.clear();
        for (std::size_t row=0; row<rows; ++row) {
            provenance.missingInformationRows.emplace_back(fields.begin()+cursor,fields.begin()+cursor+columns);
            cursor += columns;
        }
    }
    if (cursor < fields.size() && fields[cursor] == "MI_DIAGNOSTIC_ROWS_V1") {
        ++cursor;
        if (cursor >= fields.size()) return fail("Missing MI display row count.");
        char *end = nullptr;
        const long count = std::strtol(fields[cursor++].c_str(), &end, 10);
        if (!end || *end || count < 0 || static_cast<size_t>(count) > (fields.size()-cursor)/4)
            return fail("Invalid MI display rows.");
        provenance.missingInformationDisplayRows.clear();
        for (long i=0;i<count;++i) {
            const long index = std::strtol(fields[cursor++].c_str(), &end, 10);
            if (!end || *end || index < -1 || index >= static_cast<long>(provenance.missingInformationRows.size()))
                return fail("Invalid MI display row index.");
            provenance.missingInformationDisplayRows.push_back({static_cast<int>(index),fields[cursor],fields[cursor+1],fields[cursor+2]});
            cursor+=3;
        }
    }
    if (cursor < fields.size() && fields[cursor] == "FROZEN_ANALYSIS_SCOPE_V1") {
        ++cursor;
        if (cursor + 3U > fields.size()) return fail("Incomplete frozen analysis scope.");
        ImmutableAnalysisScope scope;
        scope.kind = fields[cursor++];
        scope.sourceKind = fields[cursor++];
        scope.description = fields[cursor++];
        auto count = [&](std::size_t &value) {
            if (cursor >= fields.size()) return false;
            char *end = nullptr;
            const auto parsed = std::strtol(fields[cursor++].c_str(), &end, 10);
            if (!end || *end || parsed < 0) return false;
            value = static_cast<std::size_t>(parsed); return true;
        };
        auto rows = [&](std::vector<std::string> &ids) {
            std::size_t n = 0;
            if (!count(n) || n > fields.size() - cursor) return false;
            ids.assign(fields.begin()+cursor, fields.begin()+cursor+n); cursor += n;
            return std::set<std::string>(ids.begin(),ids.end()).size() == n;
        };
        if (!count(scope.sourceN) || !rows(scope.requestedStableRowIds) ||
            !rows(scope.effectiveStableRowIds) || !rows(scope.excludedStableRowIds))
            return fail("Invalid frozen analysis scope rows.");
        scope.scopeN = scope.requestedStableRowIds.size();
        scope.effectiveN = scope.effectiveStableRowIds.size();
        std::set<std::string> requested(scope.requestedStableRowIds.begin(),scope.requestedStableRowIds.end());
        std::set<std::string> effective(scope.effectiveStableRowIds.begin(),scope.effectiveStableRowIds.end());
        if (scope.scopeN > scope.sourceN || scope.effectiveN + scope.excludedStableRowIds.size() != scope.scopeN)
            return fail("Invalid frozen analysis scope counts.");
        for (const auto &id : scope.effectiveStableRowIds)
            if (!requested.count(id)) return fail("Effective row outside frozen scope.");
        for (const auto &id : scope.excludedStableRowIds)
            if (!requested.count(id) || effective.count(id)) return fail("Invalid frozen excluded row.");
        provenance.scope = std::move(scope);
        provenance.scopeRecordedInR = true;
    }
    if (provenance.codeOrigin == RCodeOrigin::Recorded && provenance.executedRCode.empty())
        return fail("Recorded R analysis provenance contains no code.");
    return true;
}

bool ValidateDataProvenance(const DataProvenance &provenance, std::string *error)
{
    auto fail = [&](const std::string &message) { if (error) *error = message; return false; };
    if (provenance.currentVersion.datasetId.empty()) return fail("Dataset provenance has no dataset id.");
    if (provenance.currentVersion.version == 0) return fail("Dataset version must be positive.");
    if (provenance.currentVersion.storageKey.empty()) return fail("Dataset version has no stable storage key.");
    std::set<std::string> ids;
    for (const TransformationStep &step : provenance.history) {
        if (step.id.empty() || !ids.insert(step.id).second)
            return fail("Transformation ids must be non-empty and unique.");
        if (step.origin == RCodeOrigin::Recorded && step.rCode.empty())
            return fail("A recorded transformation must retain its R code.");
    }
    for (const auto &[column, stepIds] : provenance.columnSteps) {
        if (column.empty()) return fail("Column provenance has an empty column name.");
        for (const std::string &id : stepIds)
            if (!ids.count(id)) return fail("Column provenance refers to an unknown transformation.");
    }
    return true;
}

bool ValidateAnalysisProvenance(const AnalysisProvenance &provenance, std::string *error)
{
    auto fail = [&](const std::string &message) { if (error) *error = message; return false; };
    if (provenance.analysisId.empty()) return fail("Analysis provenance has no id.");
    if (provenance.dataVersion.datasetId.empty() || provenance.dataVersion.version == 0 ||
        provenance.dataVersion.storageKey.empty()) return fail("Analysis provenance has no valid data-version reference.");
    if (provenance.codeOrigin == RCodeOrigin::Recorded && provenance.executedRCode.empty())
        return fail("Recorded analysis provenance must retain the executed code.");
    if (provenance.scope.effectiveN > provenance.scope.scopeN)
        return fail("Effective N cannot exceed scope N.");
    return true;
}

bool ValidatePublicationSpec(const PublicationCodeSpec &spec, std::string *error)
{
    auto fail = [&](const std::string &message) { if (error) *error = message; return false; };
    std::set<PublicationBackend> seen;
    for (PublicationBackend backend : spec.availableBackends)
        if (!seen.insert(backend).second) return fail("Publication backend is duplicated.");
    if (spec.table) {
        for (const auto &row : spec.table->rows)
            if (row.size() != spec.table->columns.size())
                return fail("Publication table rows must match the stored column count.");
    }
    if (spec.plot) {
        for (const PublicationPlotSeries &series : spec.plot->series) {
            if (series.x.size() != series.y.size())
                return fail("Publication plot x/y vectors must have the same length.");
            if ((!series.lower.empty() && series.lower.size() != series.x.size()) ||
                (!series.upper.empty() && series.upper.size() != series.x.size()))
                return fail("Publication confidence intervals must match the plotted values.");
        }
    }
    return true;
}

} // namespace core
} // namespace rlispstat
