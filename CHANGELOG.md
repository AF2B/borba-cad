# Changelog

Format: [Keep a Changelog](https://keepachangelog.com/). The project follows semantic versioning.

## [Unreleased]

### Added

- **File dialog.** Open and Save As are one friendly screen: folders and drawings with size and
  date, places (home, documents, desktop, downloads, sample drawings), recent drawings, a clickable
  folder trail with a path field, live search, an All files switch, new folder, overwrite
  confirmation, keyboard navigation, and errors shown in the dialog with the dialog kept open.
- **Double-click distribution.** `make dist` builds a Linux folder and archive with the program, the
  libraries it needs (SDL2, FreeType and what they need), the fonts, the examples, the icon, a
  desktop launcher and install scripts; `make dist-check` verifies it in a clean environment. A
  Windows script, an MSYS2 target and a CI workflow are written but have never been run on Windows;
  the system-specific code is guarded and has portable fallbacks that the tests compare with the
  POSIX ones.
- The program takes a drawing as an argument (`borba-cad plan.bcad`), the standalone executable keeps
  its log and recent drawings in the user's state directory and finds its fonts and examples beside
  itself, and the window announces its application id so desktops show the icon.
- Drag a drawing onto the window to open it (after the usual question about unsaved changes).
- Page Up and Page Down keys; options `--state-dir` and `--drawings-dir`.
- Text fields take `Ctrl+A` (select all), `Ctrl+C`, `Ctrl+X` and `Ctrl+V` through the system clipboard,
  in the file dialog, the prompts and the inspector; pasted lines become one line.
- Typed lengths accept thousands separators in both conventions: `1.250,5` and `1,250.5` are 1250.5,
  `1.250.000` is a million and a quarter; `1,5` and `1.5` are still 1.5.

### Changed

- **New interface.** A dark theme with a teal accent, a title bar (brand mark, file and history
  buttons, document name with an unsaved-changes dot, performance toggle), a tool palette on the left
  with tooltips that show each tool's key, the layers and the properties as cards on the right, and a
  status bar with chips for the grid, the snaps, the display unit and zoom to fit. Buttons, rows,
  fields and dialogs are rounded, with soft shadows; icons and the brand mark are drawn from vector
  shapes, so they stay sharp at any size.
- Interface text is proportional (Noto Sans) and measured from the real glyph advances; drawing text
  keeps the fixed grid of the geometry module. Long text typed into a field scrolls with the caret
  instead of running out of the field, and long dialog messages wrap.

### Fixed

- Saving: a name without an extension is saved as `name.bcad`; blanks, one pair of quotes and
  `file://` addresses around a pasted path are understood instead of creating folders named `"`,
  `file:` or a blank in the current directory; saving over a symbolic link updates the file it
  points to instead of replacing the link; a replaced file keeps its permissions; saving never
  creates folders; saving over a different existing file asks first.
- Opening: a name without an extension finds the `.bcad` file; a folder, a missing file, a device or
  a blank name get a plain explanation instead of a Lisp stream error.
- The Save As and Open dialogs start in the documents folder of the desktop (`Documentos` on a
  Portuguese desktop) instead of a path relative to wherever the program was started.
- Messages report sizes in KB and MB.
- A length that could not be read was reported without the text that was typed (the message said
  `Invalid input ""` for `abc`).
- The outline of a rounded rectangle with a small corner radius was stitched with the wrong
  neighbours and drew a filled, broken shape.

## [1.0.0] - 2026-10-14

First complete version of the MVP: a native 2D CAD in Common Lisp (SBCL) with measured performance.

### Added

- **Geometry and domain:** line, rectangle, circle, polyline, text and aligned dimension entities
  with validating constructors and typed errors; layers; atomic change sets with undo and redo;
  picking, window selection, grid snap and object snap (endpoints, centers); documented tolerances
  (`docs/geometry.md`).
- **Viewport:** world/screen transforms with zoom limits, zoom at the cursor, fit to extents, and
  an adaptive grid.
- **Persistence:** versioned `.bcad` format with a safe reader (no evaluation, bounded depth and
  size, line and column in every error), exact double round trip, atomic save.
- **Application:** tools, modal prompts, numeric inspector, layers panel, status bar, and a scripted
  demonstration (data-driven) that also produces `examples/floor-plan.bcad`.
- **Window program:** SDL2 window, OpenGL 3.3 renderer driven by a draw list, FreeType glyph atlas
  with UTF-8 text, event translation, standalone executable, screenshots.
- **Observability:** rotating log with severities and per-event sampling, monotonic-clock timings,
  GC events, memory samples, live overlay, `make logs-summary`; a real session log in
  `docs/evidence/`.
- **Benchmarks:** deterministic workloads W0 to W3 with checksums; scene, pick, selection, snap,
  edit and persistence scenarios; memory soak; instrumentation overhead with a control setup; a
  windowed frame benchmark with the GPU wait; reports with an environment fingerprint; `bench.run`
  log entries; the recorded baseline in `bench/reports/baseline/`.
- **Developer workflow:** Makefile (compile, build, lint, test, test-gui, check, run, demo, debug,
  repl, exe, bench, logs, docs, example, clean), strict zero-warning build, lint for sources,
  docstrings, layout and core dependencies, environment check, API reference generator.
- **Tests:** 110,256 headless checks (including seeded randomized, fuzzing and model-based tests)
  and 131 window-program checks (PNG, SDL events, fonts, the window loop's logic, real renderer
  pixels).
- **Documents:** README, specification, architecture, geometry, performance, development guide,
  experiment report, generated API reference.

### Fixed

- The scripted demonstration stalled when its window received no event (hidden or covered), because
  its steps advanced only on frames and frames came only from events; it now requests its own
  frames. Regression tests run it without any event.

### Known limits

- Linux x86_64 and SBCL only; no commit history yet; no `LICENSE` file (the author has not chosen
  one).
- Hitches caused by garbage collection in long sessions with large drawings are not characterized
  (`docs/performance.md` §9).
- A drawing of 100,000 entities with everything in view draws at about 32 frames per second.
