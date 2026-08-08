#ifndef RLISPSTAT_CORE_RECORDING_MODEL_H
#define RLISPSTAT_CORE_RECORDING_MODEL_H

#include <ctime>
#include <string>
#include <vector>

namespace rlispstat {
namespace core {

struct RecordingEntry {
    std::time_t timestamp = 0;
    std::string group;
    std::string plotId;
    std::string description;
    std::string internalCommand;
    std::string rCode;
    bool replayable = false;
};

std::string RStringLiteral(const std::string &value);
std::string RCharacterVectorLiteral(const std::vector<std::string> &values);

std::string RecordingInternalAPICommand(const std::vector<std::string> &lines);
std::string RecordingInternalDispatchCommand(const std::string &plotId,
                                             const std::string &command);
std::vector<std::string> DecodeRecordingAPICommand(const std::string &internalCommand);

std::string RecordingDefaultRCodeForAPI(const std::vector<std::string> &lines);
std::string RecordingDescriptionForAPI(const std::vector<std::string> &lines);
bool RecordingDispatchCommandShouldRecord(const std::string &command);
bool RecordingDispatchCommandReplayable(const std::string &command);
std::string RecordingRCodeForDispatch(const std::string &command,
                                      const std::string &group,
                                      const std::string &plotId);
std::string RecordingDescriptionForDispatch(const std::string &command,
                                            const std::string &group);

std::string RecordingReportTitle();
std::string RecordingMessageTitle();
std::string RecordingSavePanelTitle();
std::string RecordingSaveDefaultFilename();
std::string RecordingSaveFailedStatus();
std::string RecordingReplayedStatus(int replayed);
std::string RecordingStartedStatus();
std::string RecordingStoppedStatus();
std::string RecordingClearedStatus();
std::string RecordingRScriptCopiedStatus();
std::string RecordingInternalCommandsCopiedStatus();

std::string RenderRecordingRScript(const std::vector<RecordingEntry> &entries,
                                   std::time_t generatedAt = 0);
std::string RenderRecordingInternalCommands(const std::vector<RecordingEntry> &entries);
std::string RenderRecordingReport(const std::vector<RecordingEntry> &entries,
                                  bool active,
                                  std::time_t generatedAt = 0);

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_RECORDING_MODEL_H
