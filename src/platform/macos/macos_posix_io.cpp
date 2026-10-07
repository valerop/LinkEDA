#include "macos_posix_io.h"

#include <fcntl.h>
#include <sys/socket.h>
#include <unistd.h>

namespace rlispstat {
namespace platform {
namespace macos {

bool ReadLine(int fd, std::string &out)
{
    out.clear();
    char ch;
    while (true) {
        ssize_t n = recv(fd, &ch, 1, 0);
        if (n <= 0) {
            return !out.empty();
        }
        if (ch == '\n') {
            return true;
        }
        if (ch != '\r') {
            out.push_back(ch);
        }
    }
}

void WriteReply(int fd, const std::string &reply)
{
    std::string msg = reply + "\n";
    const char *buf = msg.c_str();
    size_t left = msg.size();
    while (left > 0) {
        ssize_t n = send(fd, buf, left, 0);
        if (n <= 0) {
            return;
        }
        buf += n;
        left -= (size_t)n;
    }
}

void WriteReplyToFifo(const std::string &path, const std::string &reply)
{
    int fd = open(path.c_str(), O_WRONLY);
    if (fd < 0) {
        return;
    }
    std::string msg = reply + "\n";
    const char *buf = msg.c_str();
    size_t left = msg.size();
    while (left > 0) {
        ssize_t n = write(fd, buf, left);
        if (n <= 0) {
            break;
        }
        buf += n;
        left -= (size_t)n;
    }
    close(fd);
}

} // namespace macos
} // namespace platform
} // namespace rlispstat
