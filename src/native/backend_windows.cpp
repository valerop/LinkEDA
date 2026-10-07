#include "../platform/windows/linkeda_windows_backend.h"

int main(int argc, char **argv)
{
    return rlispstat::platform::windows::RunBackend(argc, argv);
}
