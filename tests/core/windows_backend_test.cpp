#include "../../src/platform/windows/linkeda_windows_backend.h"

#include <cassert>

int main()
{
    rlispstat::platform::windows::WindowsBackend backend;
    backend.createWindow("plot-1", {"Initial title", 640, 480});
    assert(backend.hasWindow("plot-1"));
    assert(backend.redrawRequested("plot-1"));
    assert(backend.windowTitle("plot-1") == "Initial title");

    backend.setWindowTitle("plot-1", "Updated title");
    backend.requestRedraw("plot-1");
    assert(backend.windowTitle("plot-1") == "Updated title");
    assert(backend.redrawRequested("plot-1"));

    backend.createWindow("plot-2", {"Second linked plot", 720, 520});
    assert(backend.hasWindow("plot-1"));
    assert(backend.hasWindow("plot-2"));
    assert(backend.windowTitle("plot-2") == "Second linked plot");

    backend.closeWindow("plot-1");
    assert(!backend.hasWindow("plot-1"));
    assert(backend.hasWindow("plot-2"));
    backend.closeWindow("plot-2");
    assert(!backend.hasWindow("plot-2"));
    return 0;
}
