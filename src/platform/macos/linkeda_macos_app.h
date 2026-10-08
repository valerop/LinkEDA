#ifndef RLISPSTAT_PLATFORM_MACOS_APP_H
#define RLISPSTAT_PLATFORM_MACOS_APP_H

namespace rlispstat {
namespace platform {
namespace macos {

// Runs the AppKit workbench backend. The executable entry point remains tiny so
// platform-specific application behavior has one clear home.
int RunBackend(int argc, char **argv);

} // namespace macos
} // namespace platform
} // namespace rlispstat

#endif // RLISPSTAT_PLATFORM_MACOS_APP_H
