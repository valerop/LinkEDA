#include "recording_model.h"

#include "command_model.h"
#include "string_utils.h"

#include <sstream>

namespace rlispstat {
namespace core {

std::string RStringLiteral(const std::string &value)
{
    std::ostringstream out;
    out << "\"";
    for (unsigned char ch : value) {
        if (ch == '\\') {
            out << "\\\\";
        } else if (ch == '"') {
            out << "\\\"";
        } else if (ch == '\n') {
            out << "\\n";
        } else if (ch == '\r') {
            out << "\\r";
        } else if (ch == '\t') {
            out << "\\t";
        } else {
            out << static_cast<char>(ch);
        }
    }
    out << "\"";
    return out.str();
}

std::string RCharacterVectorLiteral(const std::vector<std::string> &values)
{
    std::ostringstream out;
    out << "c(";
    for (std::size_t i = 0; i < values.size(); ++i) {
        if (i) out << ", ";
        out << RStringLiteral(values[i]);
    }
    out << ")";
    return out.str();
}

std::string RecordingInternalAPICommand(const std::vector<std::string> &lines)
{
    std::ostringstream out;
    out << "API";
    for (const std::string &line : lines) {
        out << "\t" << EncodeCommandField(line);
    }
    return out.str();
}

std::string RecordingInternalDispatchCommand(const std::string &plotId,
                                             const std::string &command)
{
    return "DISPATCH\t" + EncodeCommandField(plotId) + "\t" + EncodeCommandField(command);
}

std::vector<std::string> DecodeRecordingAPICommand(const std::string &internalCommand)
{
    std::vector<std::string> parts = SplitTabs(internalCommand);
    std::vector<std::string> lines;
    if (parts.empty() || parts[0] != "API") {
        return lines;
    }
    for (std::size_t i = 1; i < parts.size(); ++i) {
        lines.push_back(DecodeCommandField(parts[i]));
    }
    return lines;
}

std::string RecordingDefaultRCodeForAPI(const std::vector<std::string> &lines)
{
    if (lines.empty()) {
        return "";
    }
    const std::string &cmd = lines[0];
    if (cmd == "MODEL_SET_Y" && lines.size() >= 3) {
        return "model <- ls_model(" + RStringLiteral(lines[1]) + ")\n" +
            "ls_glm_set_dependent(model, " + RStringLiteral(lines[2]) + ")";
    }
    if (cmd == "MODEL_ADD_TERM" && lines.size() >= 3) {
        return "model <- ls_model(" + RStringLiteral(lines[1]) + ")\n" +
            "ls_glm_add_predictor(model, " + RStringLiteral(lines[2]) + ")";
    }
    if (cmd == "MODEL_REMOVE_TERM" && lines.size() >= 3) {
        return "model <- ls_model(" + RStringLiteral(lines[1]) + ")\n" +
            "ls_glm_remove_predictor(model, " + RStringLiteral(lines[2]) + ")";
    }
    if (cmd == "MODEL_CLEAR_ROLE" && lines.size() >= 3) {
        return "LinkEDA:::.rls_send(" + RCharacterVectorLiteral(lines) + ")";
    }
    if (cmd == "SET_VARIABLE_TYPE" && lines.size() >= 4) {
        return "LinkEDA:::.rls_send(" + RCharacterVectorLiteral(lines) + ")";
    }
    return "LinkEDA:::.rls_send(" + RCharacterVectorLiteral(lines) + ")";
}

std::string RecordingDescriptionForAPI(const std::vector<std::string> &lines)
{
    if (lines.empty()) return "LinkEDA API command";
    const std::string &cmd = lines[0];
    if (cmd == "MODEL_SET_Y" && lines.size() >= 3) {
        return "Set response variable `" + lines[2] + "` in `" + lines[1] + "`.";
    }
    if (cmd == "MODEL_ADD_TERM" && lines.size() >= 3) {
        return "Add model term `" + lines[2] + "` in `" + lines[1] + "`.";
    }
    if (cmd == "MODEL_REMOVE_TERM" && lines.size() >= 3) {
        return "Remove model term `" + lines[2] + "` in `" + lines[1] + "`.";
    }
    if (cmd == "MODEL_CLEAR_ROLE" && lines.size() >= 3) {
        return "Clear model role for `" + lines[2] + "` in `" + lines[1] + "`.";
    }
    if (cmd == "SET_VARIABLE_TYPE" && lines.size() >= 4) {
        return "Treat variable `" + lines[2] + "` as " + lines[3] + " in `" + lines[1] + "`.";
    }
    return "Run LinkEDA API command `" + cmd + "`.";
}

bool RecordingDispatchCommandShouldRecord(const std::string &command)
{
    if (command.empty()) return false;
    CommandRequest request = ParseMenuCommand(command);
    if (request.category == CommandCategory::Recording) return false;
    if (command == "EDIT_COPY" || command == "EDIT_COPY_SELECTION" ||
        command == "FILE_CLOSE_WORKBENCH") {
        return false;
    }
    switch (request.action) {
    case CommandAction::CopySelected:
    case CommandAction::SaveSvg:
    case CommandAction::SavePng:
    case CommandAction::SavePdf:
    case CommandAction::CopySvg:
    case CommandAction::CopyEmf:
    case CommandAction::CopyPng:
    case CommandAction::CopyPdf:
    case CommandAction::PanelShow:
    case CommandAction::PanelHide:
    case CommandAction::OpenColorPalette:
    case CommandAction::PaletteHide:
    case CommandAction::ClosePlot:
    case CommandAction::FileExportData:
        return false;
    default:
        break;
    }
    return true;
}

bool RecordingDispatchCommandReplayable(const std::string &command)
{
    if (!RecordingDispatchCommandShouldRecord(command)) return false;
    CommandRequest request = ParseMenuCommand(command);
    switch (request.action) {
    case CommandAction::FileImportData:
    case CommandAction::FileExportData:
    case CommandAction::ShowActiveDataset:
    case CommandAction::ShowVariableInformation:
    case CommandAction::ChooseLabelColumn:
        return false;
    default:
        break;
    }
    return true;
}

std::string RecordingRCodeForDispatch(const std::string &command,
                                      const std::string &group,
                                      const std::string &plotId)
{
    CommandRequest request = ParseMenuCommand(command);
    switch (request.action) {
    case CommandAction::DataSetActiveDataset:
        if (!request.args.empty()) {
            return "ls_set_active_dataset(" + RStringLiteral(request.args[0]) + ")";
        }
        break;
    case CommandAction::VariableView:
        return "ls_variables_window(" + RStringLiteral(request.args.empty() ? group : request.args[0]) + ")";
    case CommandAction::OpenGLM:
        return "model <- ls_model(" + RStringLiteral(group) + ")\nls_glm_window(model)";
    case CommandAction::OpenRegressionComparison:
        return "ls_new_regression_comparison(data = " + RStringLiteral(group) + ")";
    case CommandAction::OpenGeneralizedGLM:
        return "ls_new_generalized_linear_model(data = " + RStringLiteral(group) + ")";
    case CommandAction::OpenCountRegression:
        return "ls_new_count_regression(data = " + RStringLiteral(group) + ")";
    case CommandAction::OpenCountRegressionComparison:
        return "ls_compare_count_regression_models(...)";
    case CommandAction::OpenTable1:
        return "ls_new_table1(data = " + RStringLiteral(group) + ")";
    case CommandAction::OpenCorrelationMatrix:
        return "ls_new_correlation_matrix(data = " + RStringLiteral(group) + ")";
    case CommandAction::OpenDimensionality:
        return "ls_new_dimensionality(data = " + RStringLiteral(group) + ")";
    case CommandAction::OpenDendrogram:
        return "ls_new_quick_cluster(data = " + RStringLiteral(group) + ")";
    case CommandAction::NewHistogram:
        return "# Opened histogram dialog for " + RStringLiteral(group);
    case CommandAction::NewBoxplot:
        return "# Opened boxplot dialog for " + RStringLiteral(group);
    case CommandAction::NewBarChart:
        return "# Opened bar chart dialog for " + RStringLiteral(group);
    case CommandAction::NewScatterplot:
        return "# Opened scatterplot dialog for " + RStringLiteral(group);
    case CommandAction::NewTimeSeries:
        return "# Opened time-series dialog for " + RStringLiteral(group);
    case CommandAction::PlotTheme:
        if (!request.args.empty()) {
            return "# Plot theme: " + RStringLiteral(request.args[0]);
        }
        break;
    case CommandAction::ChangeXVariable:
        if (!request.args.empty() && !plotId.empty()) {
            return "ls_set_xvar(" + RStringLiteral(plotId) + ", " +
                RStringLiteral(request.args[0]) + ")";
        }
        break;
    case CommandAction::ChangeYVariable:
        if (!request.args.empty() && !plotId.empty()) {
            return "ls_set_yvar(" + RStringLiteral(plotId) + ", " +
                RStringLiteral(request.args[0]) + ")";
        }
        break;
    case CommandAction::SetTimeSeriesGroup:
        if (!request.args.empty() && !plotId.empty()) {
            return "ls_time_series_group(" + RStringLiteral(plotId) + ", " +
                RStringLiteral(request.args[0]) + ")";
        }
        break;
    default:
        break;
    }
    return "# LinkEDA internal command: " + RStringLiteral(command);
}

std::string RecordingDescriptionForDispatch(const std::string &command,
                                            const std::string &group)
{
    CommandRequest request = ParseMenuCommand(command);
    switch (request.action) {
    case CommandAction::DataSetActiveDataset:
        if (!request.args.empty()) {
            return "Set active dataset to `" + request.args[0] + "`.";
        }
        break;
    case CommandAction::VariableView:
        return "Open Variable View.";
    case CommandAction::OpenGLM:
        return "Open General Linear Model for `" + group + "`.";
    case CommandAction::OpenRegressionComparison:
        return "Open General Linear Model comparison for `" + group + "`.";
    case CommandAction::OpenGeneralizedGLM:
        return "Open Generalized Linear Model for `" + group + "`.";
    case CommandAction::OpenCountRegression:
        return "Open Count Model for `" + group + "`.";
    case CommandAction::OpenCountRegressionComparison:
        return "Open count regression model comparison for `" + group + "`.";
    case CommandAction::ChangeXVariable:
        if (!request.args.empty()) {
            return "Change scatterplot X variable to `" + request.args[0] + "`.";
        }
        break;
    case CommandAction::ChangeYVariable:
        if (!request.args.empty()) {
            return "Change scatterplot Y variable to `" + request.args[0] + "`.";
        }
        break;
    default:
        break;
    }
    return "Run command `" + command + "`.";
}

std::string RecordingReportTitle()
{
    return "LinkEDA Recording";
}

std::string RecordingMessageTitle()
{
    return "Recording";
}

std::string RecordingSavePanelTitle()
{
    return "Save LinkEDA Recording as R Script";
}

std::string RecordingSaveDefaultFilename()
{
    return "LinkEDA-recording.R";
}

std::string RecordingSaveFailedStatus()
{
    return "Could not save the recording.";
}

std::string RecordingReplayedStatus(int replayed)
{
    return "Replayed " + std::to_string(replayed) + " recorded actions.";
}

std::string RecordingStartedStatus()
{
    return "Recording started.";
}

std::string RecordingStoppedStatus()
{
    return "Recording stopped.";
}

std::string RecordingClearedStatus()
{
    return "Recording cleared.";
}

std::string RecordingRScriptCopiedStatus()
{
    return "R script copied to the clipboard.";
}

std::string RecordingInternalCommandsCopiedStatus()
{
    return "Internal LinkEDA commands copied to the clipboard.";
}

std::string RenderRecordingRScript(const std::vector<RecordingEntry> &entries,
                                   std::time_t generatedAt)
{
    if (generatedAt == 0) {
        generatedAt = std::time(nullptr);
    }
    std::ostringstream out;
    out << "# LinkEDA recording\n"
        << "# Generated " << generatedAt << "\n\n"
        << "library(LinkEDA)\n\n";
    if (entries.empty()) {
        out << "# No recorded actions.\n";
        return out.str();
    }
    for (std::size_t i = 0; i < entries.size(); ++i) {
        out << "# " << (i + 1) << ". " << entries[i].description << "\n";
        if (!entries[i].rCode.empty()) {
            out << entries[i].rCode << "\n\n";
        } else {
            out << "# No R rendering is available yet for this action.\n\n";
        }
    }
    return out.str();
}

std::string RenderRecordingInternalCommands(const std::vector<RecordingEntry> &entries)
{
    std::ostringstream out;
    if (entries.empty()) {
        out << "# No recorded LinkEDA commands.\n";
        return out.str();
    }
    for (std::size_t i = 0; i < entries.size(); ++i) {
        out << "# " << (i + 1) << ". " << entries[i].description
            << (entries[i].replayable ? "" : " (not replayable)")
            << "\n" << entries[i].internalCommand << "\n";
    }
    return out.str();
}

std::string RenderRecordingReport(const std::vector<RecordingEntry> &entries,
                                  bool active,
                                  std::time_t generatedAt)
{
    std::ostringstream out;
    out << "Recording: " << (active ? "on" : "off") << "\n"
        << "Recorded actions: " << entries.size() << "\n\n"
        << "R script\n"
        << "--------\n"
        << RenderRecordingRScript(entries, generatedAt) << "\n"
        << "Internal LinkEDA commands\n"
        << "---------------------------\n"
        << RenderRecordingInternalCommands(entries);
    return out.str();
}

} // namespace core
} // namespace rlispstat
