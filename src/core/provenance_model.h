#ifndef RLISPSTAT_CORE_PROVENANCE_MODEL_H
#define RLISPSTAT_CORE_PROVENANCE_MODEL_H

#include "analysis_scope.h"

#include <cstdint>
#include <map>
#include <optional>
#include <string>
#include <vector>

namespace rlispstat {
namespace core {

enum class RCodeOrigin {
    Recorded,
    Reconstructed,
    Unavailable
};

std::string RCodeOriginId(RCodeOrigin origin);
RCodeOrigin RCodeOriginFromId(const std::string &id);

struct DataVersionReference {
    std::string datasetId;
    std::string displayName;
    std::uint64_t version = 1;
    std::string objectType = "data.frame";
    int imputationCount = 0;
    // Stable key used by ls_get_data_version(). It is deliberately unrelated
    // to a window title or the active R object name.
    std::string storageKey;
};

struct TransformationStep {
    std::string id;
    std::string label;
    RCodeOrigin origin = RCodeOrigin::Unavailable;
    std::string rCode;
    std::vector<std::string> parentVersionKeys;
    std::vector<std::string> inputColumns;
    std::vector<std::string> outputColumns;
    std::vector<std::string> stableRowIds;
    std::map<std::string, std::string> parameters;
    std::map<std::string, std::string> packageVersions;
    std::optional<std::uint64_t> seed;
};

struct DataProvenance {
    DataVersionReference currentVersion;
    RCodeOrigin origin = RCodeOrigin::Unavailable;
    std::string originCode;
    std::string originDescription;
    std::vector<TransformationStep> history;
    // Column name -> ordered transformation ids. Dependencies are resolved
    // through each transformation's inputColumns.
    std::map<std::string, std::vector<std::string>> columnSteps;
};

struct ImmutableAnalysisScope {
    std::string kind = "all";
    std::string sourceKind = "all_data";
    std::string description = "All observations";
    std::vector<std::string> requestedStableRowIds;
    std::vector<std::string> effectiveStableRowIds;
    std::vector<std::string> excludedStableRowIds;
    std::size_t sourceN = 0;
    std::size_t scopeN = 0;
    std::size_t effectiveN = 0;
    std::vector<std::string> warnings;
};

struct MissingInformationDisplayRow {
    int valueRow = -1;
    std::string rowType;
    std::string label;
    std::string variable;
};

struct AnalysisProvenance {
    std::string analysisId;
    std::string title;
    DataVersionReference dataVersion;
    ImmutableAnalysisScope scope;
    // Set by the optional R trailer; legacy payloads still capture scope natively.
    bool scopeRecordedInR = false;
    RCodeOrigin codeOrigin = RCodeOrigin::Unavailable;
    // The exact statistical block captured before it was sent for execution.
    std::string executedRCode;
    // Named blocks select a concrete table/plot/result without re-deriving it
    // from presentation text.
    std::map<std::string, std::string> outputRCode;
    // Portable, user-visible verification recipes keyed by output block.
    // These are deliberately separate from executedRCode: they recalculate
    // the same quantities with public package APIs from an exported data file.
    std::map<std::string, std::string> verificationRCode;
    std::vector<std::string> verificationVariables;
    std::vector<std::string> verificationWarnings;
    std::map<std::string, std::string> packageVersions;
    std::vector<std::string> warnings;
    // Frozen R input and diagnostics captured with the fitted result.
    std::string preparedDataPath;
    std::vector<std::string> missingInformationColumns;
    std::vector<std::vector<std::string>> missingInformationRows;
    std::vector<MissingInformationDisplayRow> missingInformationDisplayRows;
};

enum class PublicationValueKind {
    Missing,
    Number,
    Text,
    Logical
};

struct PublicationValue {
    PublicationValueKind kind = PublicationValueKind::Missing;
    double number = 0.0;
    std::string text;
    bool logical = false;
};

struct PublicationTableColumn {
    std::string key;
    std::string label;
    std::string spanner;
    int decimals = -1;
    bool pValue = false;
};

struct PublicationTableSpec {
    std::string title;
    std::string subtitle;
    std::vector<PublicationTableColumn> columns;
    std::vector<std::vector<PublicationValue>> rows;
    std::vector<std::string> footnotes;
    std::vector<std::size_t> stubColumns;
};

struct PublicationPlotSeries {
    std::string id;
    std::string label;
    std::vector<double> x;
    std::vector<double> y;
    std::vector<double> lower;
    std::vector<double> upper;
};

struct PublicationPlotSpec {
    std::string kind;
    std::string title;
    std::string subtitle;
    std::string xLabel;
    std::string yLabel;
    std::string legendTitle;
    bool legendTitleVisible = true;
    std::vector<std::string> xCategoryOrder;
    std::vector<PublicationPlotSeries> series;
    bool showPoints = true;
    bool showLines = true;
    bool showConfidenceIntervals = false;
    bool showAxisTickMarks = true;
    bool showAxisTickLabels = true;
    double confidenceLevel = 0.95;
    // LinkEDA's application-wide theme at the time this output reference was
    // last synchronized.  R exporters use a public ggplot2 equivalent when
    // one exists and deliberately fall back to theme_bw() otherwise.
    std::string theme = "bw";
};

enum class PublicationBackend {
    Ggplot2,
    Tinytable,
    // Serialized documents written before the tinytable transition use the
    // same numeric table-backend value.
    Gt = Tinytable,
    Latex,
    LatexPdf
};

struct PublicationCodeSpec {
    std::vector<PublicationBackend> availableBackends;
    std::optional<PublicationTableSpec> table;
    std::optional<PublicationPlotSpec> plot;
};

struct OutputCodeReference {
    std::string outputId;
    std::string analysisId;
    std::string outputBlockId;
    std::string title;
    std::string kind;
    AnalysisProvenance provenance;
    PublicationCodeSpec publication;
};

DataVersionReference DefaultDataVersionReference(const std::string &datasetId,
                                                 const std::string &datasetType,
                                                 int imputationCount,
                                                 std::uint64_t version = 1);
std::vector<std::string> StableRowIdsForCount(const std::string &datasetId,
                                              std::size_t rowCount);
ImmutableAnalysisScope CaptureImmutableAnalysisScope(
    const AnalysisScope &scope,
    const DataVersionReference &version,
    const std::vector<std::string> &datasetStableRowIds,
    const std::vector<int> &effectiveOriginalRows = {},
    const std::vector<int> &excludedOriginalRows = {});

// Kept distinct from the command-recording formatter because both portable
// modules are linked into the native application.
std::string ProvenanceRStringLiteral(const std::string &value);
std::string RNameLiteral(const std::string &value);
std::string BuildDataProvenanceRCode(const DataProvenance &provenance);
std::string BuildColumnProvenanceRCode(const DataProvenance &provenance,
                                       const std::string &column);
std::string BuildAnalysisProvenanceRCode(const AnalysisProvenance &provenance,
                                         const std::string &outputBlockId = {});
std::string BuildAnalysisVerificationRCode(const AnalysisProvenance &provenance,
                                           const std::string &outputBlockId = {});
// Replaces the deliberately non-executable path placeholder only after the
// prepared verification RDS has been written successfully. `absolutePath`
// must be the final path returned by the platform save dialog.
bool BindAnalysisVerificationDataPath(const std::string &code,
                                      const std::string &absolutePath,
                                      std::string &boundCode);
// Wraps a verification recipe in an executable Quarto document.  Platform
// exporters use this shared representation so macOS and Windows save the same
// default artifact alongside the prepared RDS data.
std::string BuildQuartoVerificationDocument(const std::string &title,
                                             const std::string &rCode);
std::string BuildTinytablePublicationRCode(const PublicationTableSpec &spec);
std::string BuildPublicationTableHtml(const PublicationTableSpec &spec,
                                      bool apa7 = false,
                                      const std::string &tableNumber = {},
                                      const std::string &tableTitle = {});
std::string BuildPublicationTableTsv(const PublicationTableSpec &spec);
// Removes a leading automatic label such as "Table 1." from a title before
// combining it with a user-selected APA table number.
std::string PublicationTitleWithoutTableLabel(const std::string &title);
// Returns an executable ggplot2 theme expression.  Unsupported or unavailable
// third-party themes have an explicit theme_bw() fallback.
std::string Ggplot2ThemeRExpression(const std::string &theme);
std::string BuildGgplot2PublicationRCode(const PublicationPlotSpec &spec);
std::string BuildLatexPublicationRCode(const PublicationTableSpec &spec,
                                       bool compilePdf);
struct LatexPublicationOptions {
    bool apa7 = false;
    bool reportHeadings = false;
    std::string tableNumber = "1";
    std::string title;
    std::string orientation = "Automatic";
};
std::string BuildLatexPublicationRCode(const std::vector<PublicationTableSpec> &tables,
    bool compilePdf, const LatexPublicationOptions &options);

std::string BuildPublicationRCode(const OutputCodeReference &output,
                                  PublicationBackend backend);

// Reads the optional, append-only trailer emitted by the R analysis backend.
// New native clients retain the statistical source captured in R; older
// clients safely ignore the trailer because it follows the established result
// payload. The cursor is unchanged when no trailer is present.
bool ReadAnalysisProvenancePayload(const std::vector<std::string> &fields,
                                   std::size_t &cursor,
                                   AnalysisProvenance &provenance,
                                   std::string *error = nullptr);

bool ValidateDataProvenance(const DataProvenance &provenance,
                            std::string *error = nullptr);
bool ValidateAnalysisProvenance(const AnalysisProvenance &provenance,
                                std::string *error = nullptr);
bool ValidatePublicationSpec(const PublicationCodeSpec &spec,
                             std::string *error = nullptr);

} // namespace core
} // namespace rlispstat

#endif
