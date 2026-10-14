# Borba CAD

A native 2D CAD in Common Lisp (SBCL). It is an engineering experiment: can Common Lisp carry a
professional drafting application, with tested geometry, measured performance and a codebase
that stays maintainable? The program draws lines, rectangles, circles, polylines, text and
aligned dimensions on layers, with pan, zoom, grid, snap, undo/redo, a numeric property inspector
and a versioned, human-readable file format. Everything it does is measured and logged.

**Status:** version 1.0.0 of the MVP. Strict build with zero warnings, lint, 110,000+ headless
checks and 131 window checks pass; the benchmark baseline and the experiment report are recorded.
Pending: the author's review of the acceptance criteria and a license. Linux x86_64 with SBCL only.

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
| `bench`, `bench-gui`, `bench-quick` | Benchmarks, written to `bench/reports/` |
| `logs-tail`, `logs-summary` | Follow the log, aggregate it per event (no GUI needed) |
| `example`, `docs` | Regenerate `examples/floor-plan.bcad`, regenerate `docs/reference.md` |
| `clean`, `distclean` | Remove build output / also logs, screenshots and non-baseline reports |

## Using the program

The tools are in the palette on the left and on the keyboard; hover a button to see its name and key.
The title bar holds the file and history buttons, the document name (a dot marks unsaved changes)
and the performance overlay; the status bar holds the hints, the pointer position and the chips
that switch the grid, grid snap, object snap, display unit and zoom to fit. Keys: `L` line, `R` rectangle, `C` circle, `P` polyline
(Enter finishes, clicking the first vertex closes), `T` text, `D` dimension, `Esc` back to select.
Click picks an entity (Shift+click toggles), dragging on empty space selects what lies fully inside
the window, dragging a selection moves it. `Delete` removes the selection, `Ctrl+A` selects all.
Pan with the middle button or Space + left drag, zoom with the wheel (the point under the cursor
stays put), `Home` fits the drawing. `G` toggles the grid, `S` grid snap, `O` object snap
(endpoints and centers), `U` cycles the display unit. `Ctrl+Z` undoes, `Ctrl+Y` or
`Ctrl+Shift+Z` redoes. `Ctrl+N`, `Ctrl+O`, `Ctrl+S` and `Ctrl+Shift+S` manage files through an
in-app path prompt. `F3` (the **Perf** button) shows the live performance overlay.

The side panel holds two cards: the layers (add, rename, delete, recolor, show/hide, lock, set
current) and the properties, where every property of the selection is editable numerically; invalid
input is rejected with a message and the previous value is kept.

## Files

Drawings are saved as `.bcad`: UTF-8 text, one S-expression, an explicit `format-version`, doubles
that round-trip exactly. Loading never evaluates code; a malformed file is refused with its line
and column, and a failed open leaves the current drawing untouched. Saving is atomic (temporary
file, flush, rename). Canonical length unit: the millimetre. A sample is in
`examples/floor-plan.bcad`.

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
| `src/` | The core (`borba-cad`, no window or foreign-library dependency): geometry, domain, viewport, persistence, observability, scene, UI and application logic; and `src/platform/` (`borba-cad/platform`): SDL window, OpenGL renderer, FreeType atlas, event loop |
| `tests/`, `platform-tests/` | FiveAM suites (headless / window program) |
| `test-support/`, `bench/` | Seeded workload generator; benchmarks (headless and windowed) |
| `scripts/` | Bootstrap, environment check, lint, build tools |
| `docs/` | [Specification](docs/specification.md), [architecture](docs/architecture.md), [geometry](docs/geometry.md), [performance](docs/performance.md), [development guide](docs/development.md), [experiment report](docs/experiment-report.md), generated [API reference](docs/reference.md), `evidence/` (a real session log) |
| `examples/` | A sample drawing |

## Known limits

Linux x86_64 and SBCL only; single-line unrotated text, axis-aligned rectangles and aligned linear
dimensions only; window selection only; no command line, DXF or printing. No `LICENSE` file yet:
the author has not chosen one.
