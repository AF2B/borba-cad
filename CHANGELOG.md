# Changelog

Format: [Keep a Changelog](https://keepachangelog.com/). The project follows semantic versioning.

## [Unreleased]

### Added

- **Crossing selection.** Dragging on empty space from right to left selects what the rectangle
  touches (drawn dashed and green); from left to right it selects what lies inside, as before
  (solid). A diagonal line is taken only when it really passes through the rectangle, and a circle
  or a rectangle that holds the whole rectangle, without its outline reaching it, is not.
- **Copy, cut and paste of entities.** `Ctrl+C`, `Ctrl+X` and `Ctrl+V` copy, cut and paste the
  selection; `Ctrl+Shift+V` pastes in place. A paste is centered on the pointer (or on the view when
  the pointer is not over the canvas), becomes the selection so it can be moved at once, and is one
  step of undo. What is copied is the text of a small drawing in the `.bcad` format, on the system
  clipboard, so it pastes into another window of the program; it goes onto the layer of the same name
  when there is one that is visible and unlocked, and onto the current layer otherwise. Anything else
  on the clipboard is refused: the pasted text is read by the strict reader, which evaluates nothing.
- **Languages.** The interface speaks English (US), always the default, and Brazilian Portuguese. The
  globe button in the title bar opens a menu of languages; the choice is kept between runs, and
  `--language pt-BR` or `BORBA_CAD_LANGUAGE=pt-BR` override it. The English text of the program is the
  key of each message, so a text that has no translation shows in English; a catalog is a UTF-8
  data file (`src/i18n/pt-br.catalog`), and the lint fails when a text of the program has no
  translation in it or the catalog keeps one the program does not use. Messages with plurals,
  tool prompts, property names, errors and the file dialog are translated; the messages about the
  internal structure of a damaged `.bcad` file, the performance overlay, the log and the
  command-line help stay in English.
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
- A drawing with nothing in it says how to begin: a card over the canvas with the first tools and keys.
- Text fields take `Ctrl+A` (select all), `Ctrl+C`, `Ctrl+X` and `Ctrl+V` through the system clipboard,
  in the file dialog, the prompts and the inspector; pasted lines become one line.
- Typed lengths accept thousands separators in both conventions: `1.250,5` and `1,250.5` are 1250.5,
  `1.250.000` is a million and a quarter; `1,5` and `1.5` are still 1.5.

### Changed

- **One button for the drawing tools.** The palette holds the select tool and a Draw button; the
  button opens a list of the drawing tools (line, polyline, rectangle, circle, text, dimension) with
  their keys, and choosing one goes on as before. The letters L, R, C, P, T and D are unchanged. The
  list is a reusable drop-down menu: the mouse or the arrows and Enter choose, Escape or a click
  elsewhere closes it, and the rest of the window ignores the pointer while it is open.
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

- An unexpected error in the program (a bug, not something the user did) while handling an event or
  drawing a frame ended the session and lost the unsaved drawing. It is now reported in the status
  bar, logged with its call stack (`app.internal-error`), and the program goes on; a frame that
  cannot be drawn three times in a row still ends it, so a failure that repeats cannot fill the log.
  `BORBA_CAD_DEBUG=1` leaves the error to the debugger as before.
- Messages in the status bar (Saved..., Layer created...) stayed there until another one replaced
  them, which hid the hint under the pointer and the tool's prompt; they now go after six seconds
  (errors after twelve).
- A layer added or chosen out of sight in a long list is scrolled into view.
- Switching to another window while Space was held, or in the middle of a pan or a drag, left the
  program thinking Space was still down and the drag still going (the release arrives in the other
  window); losing the keyboard now lets go of both, and the pointer leaving the window clears its
  position from the status bar.
- A folder with tens of thousands of files no longer makes the file dialog filter it again for
  every widget of every frame (20,000 files: 11 ms per frame, now 1 ms).

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

- Tried on Linux x86_64 and SBCL only; no `LICENSE` file (the author has not chosen one).
- Hitches caused by garbage collection in long sessions with large drawings are not characterized
  (`docs/performance.md` §9).
- A drawing of 100,000 entities with everything in view draws at about 32 frames per second.
