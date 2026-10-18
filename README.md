# Borba CAD

**A native 2D CAD written in Common Lisp, with a Lisp console that draws.**

[![Build](https://img.shields.io/github/actions/workflow/status/AF2B/borba-cad/dist.yml?label=build)](https://github.com/AF2B/borba-cad/actions/workflows/dist.yml)
[![Release](https://img.shields.io/github/v/release/AF2B/borba-cad?include_prereleases&label=release)](https://github.com/AF2B/borba-cad/releases)
![Common Lisp](https://img.shields.io/badge/Common%20Lisp-SBCL-3a6ea5)
![Platforms](https://img.shields.io/badge/platforms-Linux%20%7C%20Windows-555)

![Borba CAD drawing a house plan, with the door selected and its grips](docs/images/main-window.png)

Borba CAD is a desktop drafting program, and an engineering experiment: can Common Lisp carry a professional
CAD application, with tested geometry, measured performance and a codebase that stays maintainable? It runs on
SBCL with SDL2 and OpenGL, saves a versioned, human-readable file format, reads and writes DXF, and lets you
draw by typing Lisp into a console, because the language of the program is Lisp itself.

## Highlights

- **Drafting**: lines, polylines, rectangles, regular polygons, circles, arcs, ellipses, text at any angle and
  aligned dimensions, on layers you can show, hide, lock and recolor.
- **Editing**: window and crossing selection, grips to stretch and turn what is selected, a numeric inspector
  for every property, copy and paste (also between windows), and undo and redo of everything.
- **Precision**: grid and object snap (endpoints and centers), millimetres inside, shown in mm, cm or m, and
  doubles that are saved and read back exactly.
- **A Lisp console**: `(rect-size 0 0 (m 9) (m 6))` draws; any Common Lisp works; a command is one step of
  undo, and one that fails leaves nothing behind. Scripts run from files and drawings export as scripts.
- **Files**: its own `.bcad` format (text, versioned, never evaluated), DXF import of any ASCII version and DXF
  R12 export, and a JPEG of what the canvas shows.
- **Friendly**: a start screen with your recent drawings, a file dialog with places and search, and the
  interface in English, Brazilian Portuguese or Russian.
- **Measured**: every operation is timed and logged; benchmarks and a session log back the numbers.

## Screenshots

| | |
|:---:|:---:|
| ![The draw menu opened from the palette](docs/images/draw-menu.png) | ![The console drawing a rosette with a loop](docs/images/console.png) |
| **One button for every drawing tool**, each with its key | **The console**: Lisp that draws, with `format`, loops and the commands |
| ![The start screen with the recent drawings](docs/images/start-screen.png) | ![The file dialog with places, recent drawings and search](docs/images/file-dialog.png) |
| **Start screen** with the recent drawings and the first things to do | **File dialog** with places, recent drawings and search |

![The interface in Brazilian Portuguese, with a rectangle selected in the inspector](docs/images/portuguese.png)
<p align="center"><b>The same drawing in Brazilian Portuguese</b>, with a rectangle selected: area and perimeter in the inspector.</p>

## Download

| System | How |
|---|---|
| **Windows** | Download the `.zip` from the [releases](https://github.com/AF2B/borba-cad/releases), extract it, double-click `borba-cad.exe`. Windows warns about an unknown app because the program is not signed: choose *More info*, then *Run anyway*. If it does not start, `borba-cad-console.exe` shows why. |
| **Linux** | Build the folder with `make dist` (see below), unpack it anywhere and double-click `Borba CAD.desktop`; `./install.sh` adds it to the applications menu and opens `.bcad` files with it. |

Both folders carry the libraries and the fonts they need, so nothing has to be installed. The Windows build is
made by [GitHub Actions](.github/workflows/dist.yml) on a Windows machine and has not yet been tried on a real
one; see [docs/development.md](docs/development.md#windows).

## Build from source

| What | Tested with |
|---|---|
| SBCL | 2.6.9 (2.4 or newer is accepted, with a warning) |
| Quicklisp | installed at `~/quicklisp`, dist `2026-01-01` (checked by `make check-env`) |
| Native libraries | SDL2 (`sdl2-compat` works), OpenGL 3.3, FreeType plus its headers, `gcc` |
| Fonts | Noto Sans and Noto Sans Mono (DejaVu or Liberation also work), or one named by `--font FILE` / `BORBA_CAD_FONT` |
| A graphical session | Wayland or X11, for the window; everything else runs headless |

```bash
make check-env   # what is installed, what is missing, and how to fix it
make test        # headless correctness suites
make run         # open the window
```

Open a drawing at startup with `make run FILE=examples/floor-plan.bcad`, change the log verbosity with
`LOG_LEVEL=debug`, pass any program option with `ARGS="..."`; the program also takes the drawing as an argument:
`borba-cad plan.bcad`.

<details>
<summary><b>All the make targets</b></summary>

`make help` lists them all. The main ones:

| Target | Purpose |
|---|---|
| `check-env` | Verify SBCL, Quicklisp dist, native libraries, font and display session |
| `compile` / `build` | Strict rebuild of every system (any warning fails) / the same, then the executable |
| `lint` | Source rules, a docstring for every exported symbol, one-parameter-per-line layout, core dependencies, translations |
| `test` / `test-gui` | Headless suites / window-program tests (renderer tests skip with a message without a display) |
| `check` | `compile`, `lint`, `test` and `test-gui` |
| `run`, `demo`, `debug`, `repl` | Run the program, a scripted demo with a screenshot, run under the Lisp debugger, an interactive session |
| `exe` / `run-exe` | Standalone executable `build/borba-cad` |
| `dist` / `dist-check` | Linux folder to double-click and its `.tar.gz` in `dist/` / start it in a clean environment and check it uses its own libraries and fonts |
| `dist-windows` | Windows folder and `.zip`, in an MSYS2 shell (the workflow runs it on GitHub) |
| `bench`, `bench-gui`, `bench-quick` | Benchmarks, written to `bench/reports/` |
| `logs-tail`, `logs-summary` | Follow the log, aggregate it per event (no GUI needed) |
| `example`, `docs` | Regenerate `examples/floor-plan.bcad`, regenerate `docs/reference.md` |
| `clean`, `distclean` | Remove build output / also logs, screenshots and non-baseline reports |

</details>

## Using the program

When it starts without a drawing, a **start screen** offers the drawings you worked on last (up to five, each
with its folder), a new drawing, a drawing to open, a script to run, or the way out; Escape goes on with an
empty drawing. The palette on the left holds the Select tool and the **Draw** button, which lists every drawing
tool with its key. The title bar has the file and history buttons, the name of the drawing (a dot marks unsaved
changes), the **More** menu (DXF, JPEG, the console and the scripts), the language menu and the performance
overlay. The side panel holds the layers and the properties, where every property of the selection is
editable as a number; input that makes no sense is refused with a message and the old value is kept. The
status bar shows the hint, the position of the pointer and the switches of the grid, the snaps, the unit and
zoom to fit.

Select one entity and its **grips** appear: squares to change its shape, a circle to move it. Click picks an
entity (Shift+click adds or removes), dragging from left to right selects what lies inside the rectangle and
from right to left (dashed) what it touches, and dragging a selection moves it.

<details>
<summary><b>Keys</b></summary>

| Keys | What they do |
|---|---|
| `L` `P` `R` `N` `C` `A` `E` `T` `D` | Line, polyline (Enter finishes, the first vertex closes), rectangle, polygon (Up and Down set the sides), circle, arc (center, start, end counter-clockwise), ellipse (center, axis end, other half-axis), text (drag to turn it), dimension |
| `Esc` | Cancel what the tool is doing, then back to Select, then clear the selection |
| `Delete`, `Ctrl+A` | Delete the selection, select all |
| `Ctrl+C`, `Ctrl+X`, `Ctrl+V`, `Ctrl+Shift+V` | Copy, cut, paste at the pointer, paste where it was copied from |
| `Ctrl+Z`, `Ctrl+Y` or `Ctrl+Shift+Z` | Undo, redo |
| Wheel, middle button or `Space`+drag, `Home` | Zoom at the pointer, pan, fit the drawing |
| `G`, `S`, `O`, `U` | Grid, grid snap, object snap, display unit |
| `Ctrl+N`, `Ctrl+O`, `Ctrl+S`, `Ctrl+Shift+S` | New, open, save, save as (a drawing dropped on the window opens too) |
| `Ctrl+Shift+I`, `Ctrl+Shift+E` | Import DXF, export DXF |
| `F2`, `F3` | Console, performance overlay |

</details>

## The console and scripts

`F2` (or More > Console) opens the console under the canvas. Type a Lisp expression and press Enter. Lengths are
millimetres and angles degrees, unless written `(rad x)`; `(mm 3)`, `(cm 20)`, `(m 5)`, `(inch 1)` and
`(foot 2)` give millimetres.

```lisp
(layer "walls" :color :orange)            ; makes the layer and draws on it from now on
(rect-size 0 0 (m 9) (m 6))               ; => 1   (every command that draws returns its id)
(dotimes (i 3) (circle (* i (m 2)) 0 300))
(text 100 100 "Living room" :height 250 :angle 15)
(move (entities :kind :circle) 0 (m 1))   ; ids, or lists of ids
(edit 2 :radius 500)                      ; the properties the inspector lists
(format t "~d entities~%" (entity-count)) ; prints in the console
(select-all) (fit) (help)
```

The commands: `line rect rect-size circle arc ellipse polygon pline text dimension` draw; `move copy erase
edit move-to-layer` change; `layer with-layer layers entities entity-count extents info` look and arrange;
`select select-all deselect selected fit` act on the window; `display-unit grid-spacing` set the drawing; `say`
and `help` talk, and `(help 'line)` explains one. Up and Down recall commands, Page Up and Page Down scroll, an
expression whose parentheses are still open waits for its next lines, `Ctrl+L` clears.

**It is native Lisp.** What you type is read and evaluated by SBCL in a package that has all of Common Lisp
and the commands, so `format`, `let`, `defun`, `loop`, `mapcar` and the rest work, and a function you define
stays defined. Numbers with a point are double-floats, and `sqrt`, `sin`, `cos`, `tan`, `asin`, `acos`,
`atan`, `exp`, `log` and `float` answer in double-floats even for a whole number (`(sqrt 2)` is
1.4142135623730951; `cl:sqrt` is still there).

**What keeps it safe:**

- **A command is one step of undo.** `Ctrl+Z` takes the whole command back. If a step fails, or the command
  runs more than 10 seconds (60 for a script file), or it makes more than two million changes, everything it
  did to the entities is put back. Layers and drawing settings are not part of that step, as in the layers
  panel. What a command prints is cut after 200,000 characters.
- **Reading never evaluates anything.** A `.bcad` file and the clipboard are never run, and the text of a
  command is read with the reader's evaluation off (`#.` is an error).
- **Scripts run only when you say so.** More > Run script... reads and checks the `.lisp` file, then asks
  where it draws: on **a new layer** of this drawing (named for the file, removed if left empty) or in **a new
  drawing**, with a reminder to run only files you trust.
- **The console is full Lisp,** with your permissions, internals and files included. That is what makes it
  useful; do not paste what you do not understand.

More > Export as script... writes the drawing as the commands that draw it again, bit for bit.
[`examples/house-plan.lisp`](examples/house-plan.lisp) and [`examples/rosette.lisp`](examples/rosette.lisp)
are scripts to read and run.

## Files and formats

- **`.bcad`**, the program's own format: UTF-8 text, one S-expression, an explicit `format-version` (2; version
  1 files open and are migrated), doubles that read back exactly. Loading never evaluates code; a malformed
  file is refused with its line and column, and a failed open leaves the open drawing untouched. Saving is
  atomic (temporary file, flush, rename).
- **DXF import** (More > Import DXF) reads an ASCII DXF of any version as a new drawing: layers, lines,
  circles, arcs, polylines, texts and multiline texts, ellipses, splines, leaders, filled shapes and 3D faces
  (as outlines), blocks (expanded, scaled, turned, mirrored, nested) and dimensions. It asks for the unit when
  the file does not say it, moves far-away drawings to the origin, and reports what it left out (hatches,
  points, images, paper space).
- **DXF export** writes R12, which every program reads: rectangles, polygons and ellipses as closed polylines,
  dimensions as lines, arrowheads and text, layers with the nearest of AutoCAD's 256 colors.
- **JPEG export** (More > Export as JPEG) saves what the canvas shows, at the view you have and as sharp as
  your display, without the panels, the selection or the cursor. The encoder is written in Lisp.

Exports end with a message in the middle of the window that says the file was saved or why it was not.

**Autosave.** While a drawing has changes that are not saved, a copy is written to the state directory every
minute (only when something changed) and taken away once the drawing is saved or closed. If the program
closes any other way, a crash or a power cut, the next start asks whether to recover it: **Recover** opens it
as work not yet saved, **Discard** throws it away, **Not now** keeps it for next time.

**The file dialog** lists folders first, then drawings with their size and date, with places (home,
documents, desktop, downloads, the sample drawings) and your recent drawings on the left. Click a step of the
path to go there, `Ctrl+L` to type a path, `Alt+Up` to go up; typing finds files as you type. The recent list
lives in the state directory (`--state-dir`); the dialog starts in the folder of the open drawing or in your
documents (`--drawings-dir`).

## Languages

English (US) is always the default. The globe in the title bar switches to **Português (Brasil)** or
**Русский (Россия)**, and the choice is kept for the next run; `--language pt-BR` or `BORBA_CAD_LANGUAGE`
override it. Each language is a data file in [`src/i18n/`](src/i18n), and the lint fails when a text of the
program has no translation.

## Observability

Every run writes a log (rotated, 5 files of 5 MiB), one line per event:

```
2026-10-14T03:22:52.259Z INFO file.load duration_ms=0.459 bytes=1462 entity_count=10 read_ms=0.155 parse_ms=0.280 result=success
```

Frames and input are aggregated once per second; `LOG_LEVEL=debug` adds every operation. `make logs-summary`
prints count, total, mean, p50, p95 and max per event. Durations come from the monotonic clock; what cannot be
measured is reported as unavailable, never estimated.

## Project layout

| Path | Contents |
|---|---|
| `src/` | The core (`borba-cad`, no window or foreign library): geometry, domain, viewport, persistence, DXF, images, the script language, observability, scene, UI and application; and `src/platform/` (`borba-cad/platform`): SDL window, OpenGL renderer, FreeType atlas, event loop |
| `tests/`, `platform-tests/` | FiveAM suites (headless / window program) |
| `test-support/`, `bench/` | Seeded workload generator; benchmarks (headless and windowed) |
| `scripts/` | Bootstrap, environment check, lint, build and packaging tools |
| `docs/` | [Specification](docs/specification.md), [architecture](docs/architecture.md), [geometry](docs/geometry.md), [performance](docs/performance.md), [development guide](docs/development.md), [experiment report](docs/experiment-report.md), generated [API reference](docs/reference.md) |
| `examples/` | A sample drawing and two scripts |

## Status and known limits

Version **1.2.0** (see the [changelog](CHANGELOG.md)). The strict build has zero warnings, and the lint,
226,000+ headless checks and 215 window checks pass. The benchmark baseline and the experiment report are
those of 1.0.0.

- Run on Linux x86_64 with SBCL; the Windows build is made by the workflow and has not yet been opened on a
  real Windows machine.
- Single-line text, axis-aligned rectangles and aligned linear dimensions only; no printing; JPEG is the only
  picture format.
- The DXF import was tried on test files and a few small files from the internet, not on a collection of real
  drawings.
- Portuguese and Russian keep the decimal point, and leave in English the console's error messages and the
  text of `help`. The Russian catalog has not been read by a native speaker.
- A console command runs on the window's thread, so the window waits for it (up to its limit), and a command
  that fails halfway leaves the layers it made.

## License

No license has been chosen yet, so all rights are reserved by the author for now.
