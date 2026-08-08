# Windows backend notes

`rlispstat` currently ships an AppKit backend in `src/native/backend_macos.mm`.
The portable pieces that must be reused by a future Windows backend live under
`src/core`.

The Windows backend should implement the neutral interface in
`src/platform/platform_backend.h`. It should translate Windows window/control
events into core commands and translate core drawing geometry into WinUI 3 with
C++/WinRT (Windows App SDK). The first milestone remains deliberately headless:
it proves the shared command and TCP boundaries with Rtools before a later
MSVC/Windows App SDK visual target is introduced.

The planned WinUI renderer uses Direct2D for points, lines, axes, and overlays;
DirectWrite for labels, titles, and scales; and WIC (or the equivalent Windows
App SDK image API) for raster image export. PDF/vector export and a full UI are
outside the headless transport milestone.

Mouse and keyboard handling should use neutral coordinates and modifiers before
calling core models. AppKit types such as `NSPoint`, `NSRect`, `NSColor`, and
`NSString` must not be introduced into `src/core`.

Reusable today:

- linked-selection semantics in `core::SelectionModel`;
- case identifiers and selection operations;
- future plot/table geometry extracted into `src/core`.

Still macOS-specific today:

- window creation and menu handling;
- AppKit drawing code;
- native table views and controllers;
- most plot geometry and hit-testing, which still lives in
  `backend_macos.mm` and should be extracted incrementally.
