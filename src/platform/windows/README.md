# Windows implementation notes

LinkEDA keeps its data, selection, analysis and command logic in `src/core`.
The Windows layer must not duplicate that logic.  `ApplicationState`,
`CommandDispatcher`, `PlotCoordinator`, model objects and the R wire protocol
are portable and are shared with macOS.

## Components

- `linkeda_windows_backend.cpp` is the Rtools-friendly TCP backend.  It
  binds only to localhost and uses Winsock instead of the macOS FIFO transport.
  It is useful for compiling and exercising the shared command/core layer.
- `windows_emf_export.cpp` provides native Enhanced Metafile clipboard export
  for vector plots and tables.
- `winui/LinkEDA` is the desktop application.  It uses WinUI 3 and
  C++/WinRT, owns the Windows event loop, and accepts the same line-oriented
  commands from R over localhost TCP.

TCP requests to WinUI end with the private
`__LINKEDA_TCP_MESSAGE_END__` record.  It is removed before command dispatch;
the public command syntax remains unchanged.  The marker is required because R
waits for the reply before closing the socket.

The WinUI process has no visible main, controller or workspace window.  Data
sheets, plots and output surfaces are independent top-level windows.  A hidden
technical owner keeps the UI dispatcher and TCP listener alive after the last
visible window closes, so a later R command can open another surface without
displaying an empty placeholder.

The data-sheet and scatterplot prototype share one `ApplicationCommandRegistry`.
Every data sheet owns a conventional client-area menu bar containing File,
Data, Plots, Analysis, Window and Help.  Its visual menu dispatches through the
shared registry with the owning data sheet's dataset as context.  Plot and
output windows have no application menu or reserved menu strip.  Window
activation updates the registry, and every data-sheet Window menu enumerates
and restores all registered top-level windows.  Plot context menus and keyboard
interactions remain local to the plot; they also provide Show data sheet so a
closed sheet can be reopened while its plots remain visible.

The current document surfaces are the linked scatterplot and data-sheet views.
The scatterplot draws points and axes with WinUI primitives, supports
click and rectangular brushing, and refreshes every open plot in the same
selection group through `PlotCoordinator`.  The data sheet renders the dataset
registered by R and links extended row selection back to the plots.  Programmatic
selection, group/plot queries, redraw and close commands use the same dispatcher
state.  Commands outside the lightweight scatter session are dispatched to the
shared `CommandDispatcher`, so they retain common validation and state rather
than creating a Windows-only command implementation.

## Build the WinUI application

The end-user Windows download is built with
`scripts/build-windows-download.ps1`; see `docs/windows-download.md`. That
release bundles an unpackaged, self-contained WinUI application inside a
Windows-only R package. Users install it from R without Visual Studio, Rtools,
MSIX registration, or Developer Mode. The steps below are for local development
of the separately registered MSIX application.

1. Install Visual Studio 2022 or newer with **Desktop development with C++**,
   the Windows 10/11 SDK, MSIX tools, and C++/WinRT.
2. Restore the NuGet packages declared in `winui/LinkEDA/packages.config`.
3. Open `winui/LinkEDA/LinkEDA.slnx` in Visual Studio and build
   `Debug|x64` or `Release|x64`.
4. Deploy/install the generated MSIX package. For a local development build,
   register the Release manifest after every build whose version changes:

   ```powershell
   Add-AppxPackage -Register .\winui\LinkEDA\bin\x64\Release\AppxManifest.xml -ForceApplicationShutdown
   ```

   `R CMD INSTALL .` installs only the R package and its optional Rtools
   fallback; it does not deploy the WinUI MSIX application. The R package
   starts the installed app through `LinkEDA.winui_app_id`; configure that
   option if your deployment uses a different app identity.
5. Install the R package with Rtools from the package root:

   ```sh
   R CMD INSTALL .
   ```

When the WinUI application is installed, R connects to or starts it before it
looks for the optional Rtools console backend.  The console executable is only
a fallback and is not required for normal WinUI use.  Startup also probes the
shared command protocol, so an obsolete registered development build is
rejected instead of being mistaken for the current application.
Set `options(LinkEDA.use_winui = FALSE)` to force the Rtools TCP backend for
headless protocol/core checks.

## Platform boundaries

Mac-specific AppKit, Objective-C++ and POSIX FIFO code remain under
`src/platform/macos`.  Windows-only Win32, Winsock, EMF and WinUI code stays
under this directory.  AppKit and Win32 value types must not enter `src/core`.
