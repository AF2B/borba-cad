# Borba CAD

A native 2D CAD in Common Lisp (SBCL). It is an engineering experiment: can Common Lisp carry a
professional drafting application, with tested geometry, measured performance and a codebase
that stays maintainable? The program draws lines, rectangles, circles, arcs, ellipses, regular
polygons, polylines, text (at any angle) and aligned dimensions on layers, with pan, zoom, grid,
snap, undo/redo, grips, copy and paste, a numeric property inspector, DXF import and export, a
versioned, human-readable file format, and a Lisp console that draws and edits with commands.
Everything it does is measured and logged.

**Status:** version 1.1.0 (see the [changelog](CHANGELOG.md)): the MVP of 1.0.0 plus a new
interface, a file dialog, a Linux double-click distribution, English and Brazilian Portuguese,
copy and paste, crossing selection, grips, arcs, ellipses, polygons, rotated text, DXF import and
export, and the console with its scripts. Strict build with zero warnings, lint, 134,000+ headless
checks and 200+ window checks pass; the benchmark baseline and the experiment report are those of
1.0.0 and were not redone for 1.1.0. Pending: the author's review of the acceptance criteria and a
license. Linux x86_64 with SBCL is the only platform that has been built and tried; the Windows
packaging is written and has never run, and the DXF import has been checked on files made for the
tests, not on a collection of real ones.

## Requirements

| What | Tested with |
|---|---|
| SBCL | 2.6.9 (2.4 or newer is accepted, with a warning) |
| Quicklisp | installed at `~/quicklisp`, dist `2026-01-01` (checked by `make check-env`) |
| Native libraries | SDL2 (`sdl2-compat` works), OpenGL, FreeType plus its headers, `gcc` |
| Fonts | Noto Sans and Noto Sans Mono (DejaVu or Liberation also work), or a monospace font named by `--font FILE` / `BORBA_CAD_FONT` |
| A graphical session | Wayland or X11, for the window; everything else runs headless |

## Quick start

```bash
make check-env   # what is installed, what is missing, and how to fix it
make test        # headless correctness suites
make run         # open the window
```

Open a drawing at startup with `make run FILE=examples/floor-plan.bcad`, change the log verbosity
with `LOG_LEVEL=debug`, pass any program option with `ARGS="..."`.

## Targets

`make help` lists them all. The main ones:

| Target | Purpose |
|---|---|
| `check-env` | Verify SBCL, Quicklisp dist, native libraries, font and display session |
| `compile` / `build` | Strict rebuild of every system (any warning fails) / the same, then the executable |
| `lint` | Source rules, a docstring for every exported symbol, one-parameter-per-line layout, core dependencies |
| `test` / `test-gui` | Headless suites / window-program tests (renderer tests skip with a message without a display) |
| `check` | `compile`, `lint`, `test` and `test-gui` |
| `run`, `demo`, `debug`, `repl` | Run the program, a scripted demo with a screenshot, run under the Lisp debugger, an interactive session |
| `exe` / `run-exe` | Standalone executable `build/borba-cad` |
| `dist` / `dist-check` | Linux folder to double-click and its `.tar.gz` in `dist/` / start it in a clean environment and check it uses its own libraries and fonts |
| `dist-windows` | Windows folder and `.zip` (in an MSYS2 shell; written but never run on Windows) |
| `bench`, `bench-gui`, `bench-quick` | Benchmarks, written to `bench/reports/` |
| `logs-tail`, `logs-summary` | Follow the log, aggregate it per event (no GUI needed) |
| `example`, `docs` | Regenerate `examples/floor-plan.bcad`, regenerate `docs/reference.md` (the scripts in `examples/` are written by hand) |
| `clean`, `distclean` | Remove build output / also logs, screenshots and non-baseline reports |

## Running it by double-click

`make dist` makes `dist/borba-cad-VERSION-linux-x86_64/` and a `.tar.gz`. Unpack it anywhere and
double-click `Borba CAD.desktop` (or `borba-cad`): the folder carries the libraries and the fonts
it needs, so nothing has to be installed. `./install.sh` adds it to the applications menu and makes
a double click on a `.bcad` file open it. The Windows folder (`make dist-windows`, in an MSYS2
shell, or the workflow in `.github/workflows/dist.yml`) has the same idea with `borba-cad.exe`, but
has never been built on Windows: see docs/development.md, "Windows". The program also takes the
drawing as an argument: `borba-cad plan.bcad`.

## Using the program

When the program starts without a drawing it shows a **start screen** in the middle of the window: the
drawings you worked on last (up to five, the latest first, with the folder each is in), and New drawing,
Open a drawing..., Run a script... and Quit. The arrows and Enter, or the pointer, choose; Escape goes on
with the empty drawing. Starting with a file (`borba-cad plan.bcad`) skips it.

The Select tool and the Draw button, which opens the list of drawing tools, are in the palette on the left; every tool also has a key, shown in the list.
The title bar holds the file and history buttons, the document name (a dot marks unsaved changes),
the More menu (DXF, a JPEG of the canvas, the console and the scripts), the language menu (English (US), the default, or
Portuguese (Brazil), kept for the next run) and the performance overlay; the status bar holds the hints, the pointer position and the chips
that switch the grid, grid snap, object snap, display unit and zoom to fit. Keys: `L` line, `R` rectangle, `C` circle, `P` polyline
(Enter finishes, clicking the first vertex closes), `A` arc (center, start, direction of the end),
`E` ellipse (center, end of an axis, the other half-axis), `N` regular polygon (center, a vertex; Up and
Down set the number of sides), `T` text (drag to turn it), `D` dimension, `Esc` back to select.
Select one entity and its grips appear (squares to change the shape, a circle to move it): drag one
to stretch a line, resize a rectangle, circle or polygon, move a vertex, turn a text or an ellipse
axis; Escape abandons the drag. Click picks an entity (Shift+click toggles), dragging on empty space from left to right selects
what lies fully inside the rectangle and from right to left (dashed) what it touches, dragging a
selection moves it. `Delete` removes the selection, `Ctrl+A` selects all.
`Ctrl+C`, `Ctrl+X` and `Ctrl+V` copy, cut and paste the selection (centered on the pointer;
`Ctrl+Shift+V` pastes where it was copied from), also into another window of the program.
Pan with the middle button or Space + left drag, zoom with the wheel (the point under the cursor
stays put), `Home` fits the drawing. `G` toggles the grid, `S` grid snap, `O` object snap
(endpoints and centers), `U` cycles the display unit. `Ctrl+Z` undoes, `Ctrl+Y` or
`Ctrl+Shift+Z` redoes. `Ctrl+N`, `Ctrl+O`, `Ctrl+S` and `Ctrl+Shift+S` manage files; Open and Save
As show the file dialog (below), and a drawing dropped on the window opens. `F3` (the **Perf**
button) shows the live performance overlay.

The side panel holds two cards: the layers (add, rename, delete, recolor, show/hide, lock, set
current) and the properties, where every property of the selection is editable numerically; invalid
input is rejected with a message and the previous value is kept.

## Files

Drawings are saved as `.bcad`: UTF-8 text, one S-expression, an explicit `format-version` (2; files
of version 1 open and are migrated), doubles that round-trip exactly. Loading never evaluates code; a malformed file is refused with its line
and column, and a failed open leaves the current drawing untouched. Saving is atomic (temporary
file, flush, rename). Canonical length unit: the millimetre. A sample is in
`examples/floor-plan.bcad`.

**The file dialog** is one screen for choosing where a drawing is saved and which one is opened. It
lists the folder (folders first, then drawings with their size and date), with the places on the
left (home, documents, desktop and downloads in your language, the sample drawings) and your
recent drawings under them. Click a step of the path above the list to go there, the pencil (or
`Ctrl+L`) to type a path, the arrow (or `Alt+Up`, or `Backspace` in an empty search) to go up one
folder. Typing finds files by name as you type; `Up`, `Down`, `Page Up` and `Page Down` move the
selection, `Enter` opens or saves, a double click opens a drawing or enters a folder, `Esc`
closes. Saving takes a name (`.bcad` is added), can create a folder (`Ctrl+Shift+N`) and asks
before replacing a different file. **All files** also lists files that are not drawings. When a
file cannot be opened or saved the dialog stays open and says why. The recent list is kept in
`var/state/recent-files.txt` (`--state-dir`); the dialog starts in the folder of the open drawing,
or in your documents folder (`--drawings-dir`).

## DXF and pictures

The More button of the title bar (three dots) has **Import DXF** (`Ctrl+Shift+I`) and **Export DXF**
(`Ctrl+Shift+E`), which open the file dialog for `.dxf` files.

- **Import** reads an ASCII DXF of any version as a new drawing that is not saved yet: layers (with name,
  color, off, locked), lines, circles, arcs, polylines (arc segments become short straight ones), texts
  and multiline texts, ellipses, splines (as polylines of short segments), filled shapes and 3D faces (as
  outlines), leaders (without arrowheads), blocks (inserts are expanded, scaled, turned, mirrored,
  repeated, nested) and dimensions (as what their block draws). A file that does not say its unit asks for
  it; one far from the origin (survey coordinates) is moved to it. What the program cannot draw (hatches,
  points, images, solids in 3D) and what is in paper space is left out, and a report says how many of each;
  it also goes to the log.
- **Export** writes DXF version R12, which every program reads: lines, circles, arcs, texts and polylines
  as themselves; rectangles, polygons and ellipses as closed polylines; dimensions as lines, arrowheads
  and text; layers with their state and the nearest of AutoCAD's 256 colors. The status bar says what
  changed shape.

**Export as JPEG** (More menu) saves a picture of **what the canvas shows**: the rectangle where the grid and
the drawing are, at the view you have, as sharp as your display (twice the size on a screen that doubles its
pixels). The panels, the selection, the tool in use and the cursor are not in it. The encoder is part of the
program (baseline JPEG, quality 90), so it needs no library.

The `.bcad` file stays the program's own format: it keeps rectangles, polygons, ellipses and dimensions as
what they are.

## The console and scripts

`F2` (or More > Console) opens the console under the canvas; `F2` or the cross closes it. Type a Lisp
expression and press Enter. Numbers are millimetres and angles are degrees, unless a number is
written `(rad x)`; `(mm 3)`, `(cm 20)`, `(m 5)`, `(inch 1)` and `(foot 2)` give millimetres.

```lisp
(layer "walls" :color :orange)            ; makes the layer and draws on it from now on
(rect-size 0 0 (m 9) (m 6))               ; => 1   (every command that draws returns its id)
(dotimes (i 3) (circle (* i (m 2)) 0 300))
(text 100 100 "Living room" :height 250 :angle 15)
(move (entities :kind :circle) 0 (m 1))   ; ids, or lists of ids
(edit 2 :radius 500)                      ; the properties the inspector lists
(select-all) (fit) (help)
```

The commands: `line rect rect-size circle arc ellipse polygon pline text dimension` draw; `move copy
erase edit move-to-layer` change; `layer with-layer layers entities entity-count extents info`
look and arrange; `select select-all deselect selected fit` act on the window; `display-unit
grid-spacing` set the drawing; `say` and `help` talk. `(help 'line)` explains one; everything else
is Common Lisp, so loops, functions and arithmetic work. Up and Down recall the commands run, Page
Up and Page Down scroll the output, a command whose parentheses are not closed waits for its next
lines, a paste is made one line without its comments, `Ctrl+L` clears the output.

What makes it safe to use:

- **A command is one step of undo.** It acts on the drawing as it goes, so the next step sees the
  last, but `Ctrl+Z` takes the whole command back, and if a step fails, or the command takes more
  than 10 seconds (60 for a script file), or it makes more than two million changes, everything it
  did to the entities is put back and nothing is left half done. Layers and the settings of the
  drawing a command changes are not part of that step, as in the layers panel. What a command
  prints is cut after 200,000 characters.
- **Reading never evaluates anything.** A drawing file (`.bcad`) and the clipboard are never run;
  the text of a command is read with the reader's evaluation off (`#.` is an error), as numbers
  that are double-floats.
- **Scripts run only when you say so.** More > Run script... opens a file dialog for `.lisp` files;
  the file is read and checked, and a question asks where it draws: on **a new layer** of the
  drawing you are editing (named for the file, and taken away again if the script left nothing on it),
  or in **a new drawing** that leaves yours alone (after the usual offer to save unsaved work), with a
  line that says to run only files you trust. Only then does it run, as one command with its own limit.
- **What you type in the console is full Lisp,** with the permissions of whoever runs the program:
  that is what makes it useful, and it is why a restricted dialect was not offered in its place.
  Do not paste what you do not understand.

More > Export as script... writes the drawing as the commands that draw it again, bit for bit (layers
with their colors, visibility and locks, each entity on its layer, in order), a file that Run script
reads. Exporting a script, like exporting a DXF file, ends with a message in the middle of the
window that says it worked or why it did not. `examples/house-plan.lisp` and `examples/rosette.lisp` are scripts to read and run.

## Observability

Every run writes `var/log/borba-cad.log` (rotated, 5 files of 5 MiB by default), one line per event:

```
2026-10-14T03:22:52.259Z INFO file.load duration_ms=0.459 bytes=1462 entity_count=10 read_ms=0.155 parse_ms=0.280 result=success
```

At the default level, frames and input are aggregated once per second; `LOG_LEVEL=debug` adds the
per-operation entries. `make logs-summary` prints count, total, mean, p50, p95 and max per event.
Durations come from the monotonic clock; a value that cannot be measured is reported as
unavailable, never estimated.

## Layout

| Path | Contents |
|---|---|
| `src/` | The core (`borba-cad`, no window or foreign-library dependency): geometry, domain, viewport, persistence, DXF, the script language, observability, scene, UI and application logic; and `src/platform/` (`borba-cad/platform`): SDL window, OpenGL renderer, FreeType atlas, event loop |
| `tests/`, `platform-tests/` | FiveAM suites (headless / window program) |
| `test-support/`, `bench/` | Seeded workload generator; benchmarks (headless and windowed) |
| `scripts/` | Bootstrap, environment check, lint, build tools |
| `docs/` | [Specification](docs/specification.md), [architecture](docs/architecture.md), [geometry](docs/geometry.md), [performance](docs/performance.md), [development guide](docs/development.md), [experiment report](docs/experiment-report.md), generated [API reference](docs/reference.md), `evidence/` (a real session log) |
| `examples/` | A sample drawing and two scripts |

## Known limits

Tried on Linux x86_64 with SBCL only (the Windows folder and CI workflow are written and have never
run); single-line text, axis-aligned rectangles and aligned linear dimensions only; DXF import leaves out
hatches, points, images and paper space, and export writes R12 (an ellipse, a polygon and a
dimension leave as polylines, lines and text), and the import was tried on files made for the tests;
Brazilian Portuguese keeps the decimal point in numbers and leaves in English the messages about the
structure of a damaged file, the errors the console prints (Lisp's own are English) and the text the
`help` command prints; a command or a script that fails halfway leaves the layers it made; a command
runs on the window's thread, so the window waits for it (up to its limit); no printing; the only picture format is JPEG. No `LICENSE`
file yet: the author has not chosen one.
