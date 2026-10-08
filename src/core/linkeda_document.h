#pragma once

#include "application_state.h"
#include "window_note_model.h"

#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace rlispstat::core {

inline constexpr const char *kLinkEDADataDocumentFormat = "LinkEDA";
inline constexpr std::uint32_t kLinkEDADataDocumentVersion = 10;
inline constexpr const char *kLinkEDADataDocumentSchema = "LinkEDADataDocument/v10";

struct LinkEDADataDocument {
    std::string format = kLinkEDADataDocumentFormat;
    std::uint32_t version = kLinkEDADataDocumentVersion;
    DataFrameModel dataset;
    std::map<std::string, std::string> variable_roles;
    std::vector<std::pair<int, std::string>> row_colors;
    AnalysisScope analysis_scope;
    std::vector<SavedSelection> saved_selections;
    std::vector<int> excluded_rows;
    std::vector<NamedColorScheme> color_schemes;
    std::string point_label_column;
    std::optional<WindowNote> data_sheet_note;
    std::vector<OutputCodeReference> output_code_references;
    // Shared historical versions referenced by outputs. Each version occurs
    // at most once in the document, regardless of how many outputs use it.
    std::vector<DataFrameModel> shared_data_versions;
};

bool ValidateLinkEDADataDocument(const LinkEDADataDocument &document,
                                 std::string *error = nullptr);
bool CreateLinkEDADataDocument(const ApplicationState &state,
                               const std::string &datasetId,
                               LinkEDADataDocument &document,
                               std::string *error = nullptr);
bool ApplyLinkEDADataDocument(const LinkEDADataDocument &document,
                              ApplicationState &state,
                              std::string *error = nullptr);

std::vector<unsigned char> EncodeLinkEDADataDocument(
    const LinkEDADataDocument &document, std::string *error = nullptr);
// Stable comparison payload for changes to the dataset and its variable roles.
// Analysis outputs, selections, colours, notes, and display settings do not
// make a dataset dirty when the application closes.
std::vector<unsigned char> EncodeLinkEDADataChangePayload(
    const LinkEDADataDocument &document, std::string *error = nullptr);
bool DecodeLinkEDADataDocument(const std::vector<unsigned char> &payload,
                               LinkEDADataDocument &document,
                               std::string *error = nullptr);

// Platform adapters launch these scripts with Rscript. The file itself is an
// RDS object containing an explicit LinkEDA envelope and the stable core
// payload. The adapters own only process/file-dialog plumbing.
std::string NativeLinkEDAWriteRScript();
std::string NativeLinkEDAReadRScript();

} // namespace rlispstat::core
