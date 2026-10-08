#include "../../src/core/recording_model.h"

#include <cassert>
#include <string>
#include <vector>

using rlispstat::core::DecodeRecordingAPICommand;
using rlispstat::core::RecordingDescriptionForAPI;
using rlispstat::core::RecordingDescriptionForDispatch;
using rlispstat::core::RecordingDispatchCommandReplayable;
using rlispstat::core::RecordingDispatchCommandShouldRecord;
using rlispstat::core::RecordingEntry;
using rlispstat::core::RecordingInternalAPICommand;
using rlispstat::core::RecordingInternalDispatchCommand;
using rlispstat::core::RecordingClearedStatus;
using rlispstat::core::RecordingInternalCommandsCopiedStatus;
using rlispstat::core::RecordingMessageTitle;
using rlispstat::core::RecordingRCodeForDispatch;
using rlispstat::core::RecordingReportTitle;
using rlispstat::core::RecordingReplayedStatus;
using rlispstat::core::RecordingRScriptCopiedStatus;
using rlispstat::core::RecordingSaveDefaultFilename;
using rlispstat::core::RecordingSaveFailedStatus;
using rlispstat::core::RecordingSavePanelTitle;
using rlispstat::core::RecordingStartedStatus;
using rlispstat::core::RecordingStoppedStatus;
using rlispstat::core::RenderRecordingInternalCommands;
using rlispstat::core::RenderRecordingReport;
using rlispstat::core::RenderRecordingRScript;

static bool contains(const std::string &text, const std::string &needle)
{
    return text.find(needle) != std::string::npos;
}

int main()
{
    assert(RecordingReportTitle() == "LinkEDA Recording");
    assert(RecordingMessageTitle() == "Recording");
    assert(RecordingSavePanelTitle() == "Save LinkEDA Recording as R Script");
    assert(RecordingSaveDefaultFilename() == "LinkEDA-recording.R");
    assert(RecordingSaveFailedStatus() == "Could not save the recording.");
    assert(RecordingReplayedStatus(3) == "Replayed 3 recorded actions.");
    assert(RecordingStartedStatus() == "Recording started.");
    assert(RecordingStoppedStatus() == "Recording stopped.");
    assert(RecordingClearedStatus() == "Recording cleared.");
    assert(RecordingRScriptCopiedStatus() == "R script copied to the clipboard.");
    assert(RecordingInternalCommandsCopiedStatus() ==
           "Internal LinkEDA commands copied to the clipboard.");

    std::vector<std::string> lines = {"MODEL_SET_Y", "cars", "mpg"};
    std::string encoded = RecordingInternalAPICommand(lines);
    assert(encoded == "API\tMODEL_SET_Y\tcars\tmpg");
    assert(DecodeRecordingAPICommand(encoded) == lines);

    std::vector<std::string> escaped = {"SET_VARIABLE_TYPE", "cars", "a|b", "factor"};
    std::string escapedEncoded = RecordingInternalAPICommand(escaped);
    assert(DecodeRecordingAPICommand(escapedEncoded) == escaped);

    assert(contains(RecordingDescriptionForAPI(lines), "Set response variable"));
    assert(contains(RecordingRCodeForDispatch("DATA_SET_ACTIVE_DATASET|cars", "mtcars", ""),
                    "ls_set_active_dataset"));
    assert(contains(RecordingDescriptionForDispatch("GLM", "cars"), "General Linear Model"));
    assert(contains(RecordingInternalDispatchCommand("plot1", "CHANGE_X_VARIABLE|wt"), "DISPATCH"));

    assert(!RecordingDispatchCommandShouldRecord("RECORD_START"));
    assert(!RecordingDispatchCommandShouldRecord("SAVE_PNG"));
    assert(!RecordingDispatchCommandShouldRecord("SAVE_SVG"));
    assert(!RecordingDispatchCommandShouldRecord("COPY_SVG"));
    assert(RecordingDispatchCommandShouldRecord("GLM"));
    assert(!RecordingDispatchCommandReplayable("FILE_IMPORT_DATA"));
    assert(!RecordingDispatchCommandShouldRecord("FILE_EXPORT_DATA"));
    assert(!RecordingDispatchCommandReplayable("FILE_EXPORT_DATA"));
    assert(RecordingDispatchCommandReplayable("GLM"));

    RecordingEntry entry;
    entry.description = "Open model.";
    entry.internalCommand = RecordingInternalDispatchCommand("", "GLM");
    entry.rCode = RecordingRCodeForDispatch("GLM", "cars", "");
    entry.replayable = true;

    std::vector<RecordingEntry> entries = {entry};
    std::string r = RenderRecordingRScript(entries, 123);
    assert(contains(r, "# Generated 123"));
    assert(contains(r, "library(LinkEDA)"));
    assert(contains(r, "ls_glm_window"));

    std::string internal = RenderRecordingInternalCommands(entries);
    assert(contains(internal, "DISPATCH"));
    assert(!contains(internal, "not replayable"));

    std::string report = RenderRecordingReport(entries, true, 123);
    assert(contains(report, "Recording: on"));
    assert(contains(report, "Recorded actions: 1"));

    return 0;
}
