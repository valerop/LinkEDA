#include "../platform/macos/linkeda_macos_app.h"

int main(int argc, char **argv)
{
    return rlispstat::platform::macos::RunBackend(argc, argv);
}
