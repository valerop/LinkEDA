#ifndef RLISPSTAT_PLATFORM_MACOS_POSIX_IO_H
#define RLISPSTAT_PLATFORM_MACOS_POSIX_IO_H

#include <string>

namespace rlispstat {
namespace platform {
namespace macos {

bool ReadLine(int fd, std::string &out);
void WriteReply(int fd, const std::string &reply);
void WriteReplyToFifo(const std::string &path, const std::string &reply);

} // namespace macos
} // namespace platform
} // namespace rlispstat

#endif // RLISPSTAT_PLATFORM_MACOS_POSIX_IO_H
