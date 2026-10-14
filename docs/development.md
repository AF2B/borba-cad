# Borba CAD — Development guide

How to set up, build, test, run, debug and extend the project. Everything here is a command or a
rule that the tooling in the repository supports; `make help` lists the targets.

## 1. Setup

| Need | Notes |
|---|---|
| Linux x86_64 | Developed and measured on Arch Linux (kernel 7.2.9) with a KDE Plasma Wayland session. |
| SBCL 2.4 or newer | 2.6.9 is the tested version; `make check-env` warns about any other. Arch: `pacman -S sbcl`. |
| Quicklisp | At `~/quicklisp` (override with `QUICKLISP_SETUP=...`), dist `2026-01-01`. See quicklisp.org for the installer. The dist is pinned: `make check-env` warns when it differs, and `(ql-dist:install-dist "http://beta.quicklisp.org/dist/quicklisp/2026-01-01/distinfo.txt" :replace t)` selects it (the standard Quicklisp procedure, not exercised on this machine, where the dist was already installed). |
| SDL2 | `libSDL2-2.0.so.0`; on Arch `sdl2-compat` (an SDL2 API over SDL3) works and is what was tested. |
| OpenGL 3.3 core | `libGL.so.1` from the GPU driver. |
| FreeType | `libfreetype.so.6` and its headers (`/usr/include/freetype2/ft2build.h`), plus `gcc`: `cl-freetype2` compiles a small C helper on first load. Arch: `pacman -S freetype2 gcc`. |
| A monospace font | Noto Sans Mono (`/usr/share/fonts/noto/NotoSansMono-Regular.ttf`), DejaVu Sans Mono, Liberation Mono or FreeMono are found automatically; otherwise `--font FILE` or `BORBA_CAD_FONT`. |
| A graphical session | Only for `run`, `demo`, `debug`, `test-gui` (renderer tests) and `bench-gui`. |

```bash
make check-env    # one line per requirement, with the fix when something is missing
make deps         # downloads and compiles the third-party libraries, lists their versions
make check        # strict build, lint, headless tests, window tests
make run          # start the program
```

The third-party systems are `cffi`, `cl-opengl`, `cl-freetype2`, `sdl2` and `fiveam`, plus their
dependencies, all taken from the pinned dist. The core system `borba-cad` has no third-party
dependency at all: only SBCL's own `sb-posix`.

## 2. The daily loop

| To | Run |
|---|---|
| Try the program | `make run`, `make run FILE=examples/floor-plan.bcad LOG_LEVEL=debug`, `make demo` (scripted, leaves `var/screenshots/demo.png`) |
| Work interactively | `make repl`: a REPL with the project loaded in the package `BORBA-CAD/USER`, which has the nicknames `base sbcl geo dom view persist obs scene ui app`. `REPL_SYSTEMS="borba-cad borba-cad/platform"` chooses what is loaded. |
| Run one suite | `make test SUITE=persistence` (names in §5) |
| Run everything | `make check` (about 12 s) |
| Debug a crash | `make debug`: runs under the Lisp debugger (`BORBA_CAD_DEBUG=1`), with debug logging on stderr |
| Read the log | `make logs-tail`, `make logs-summary` (count, total, mean, p50, p95, max per event) |
| Build the executable | `make exe` (67 MB, `build/borba-cad`), `make run-exe` |
| Regenerate generated files | `make docs` (`docs/reference.md`), `make example` (`examples/floor-plan.bcad`) |
| Start clean | `make clean` (compiled files), `make distclean` (also logs, screenshots, non-baseline reports) |

All compiled output goes to `build/fasl/`, logs to `var/log/`, benchmark reports to
`bench/reports/`; all three are ignored by git (`bench/reports/baseline/` is kept). Targets share
`build/`, so `make` is never run in parallel (the Makefile forbids it).

### Command-line options and environment

`make run ARGS="..."` and the executable accept:

| Option | Environment variable | Meaning |
|---|---|---|
| `--file PATH` | | Open a drawing at startup |
| `--log-dir DIR` | `BORBA_CAD_LOG_DIR` | Directory of the rotating log (default `var/log` in the project) |
| `--log-level LEVEL` | `BORBA_CAD_LOG_LEVEL` | `error`, `warn`, `info` (default), `debug` or `trace` |
| `--log-stderr` | `BORBA_CAD_LOG_STDERR` | Also print log lines on standard error |
| `--log-sample EVENT=N` | `BORBA_CAD_LOG_SAMPLING` | Write only one in N entries of an event |
| | `BORBA_CAD_LOG_MAX_BYTES`, `BORBA_CAD_LOG_MAX_FILES` | Rotation: 5 MiB and 5 files by default (at most 25 MiB on disk) |
| `--font FILE` | `BORBA_CAD_FONT` | Monospace TrueType font |
| `--width N`, `--height N` | | Initial window size in logical pixels (1440 × 900) |
| `--demo`, `--exit-after-demo`, `--screenshot FILE` | | Run the scripted demonstration, close when done, save the last frame as a PNG |
| | `BORBA_CAD_ROOT` | Project root: where `var/log` and `examples` are looked for; defaults to the directory of `borba-cad.asd` |
| | `BORBA_CAD_DEBUG=1` | Do not catch unexpected errors, so a debugger can take them |

Exit status: 0 on a normal exit, 2 for a bad option, 1 for any other failure.

## 3. Layout and layering

```
src/sbcl-support  src/base  src/geometry  src/domain  src/viewport  src/persistence
src/observability src/scene src/ui        src/app                          -> system borba-cad
src/platform                                                               -> borba-cad/platform
test-support/  tests/  platform-tests/  bench/  scripts/  docs/  examples/
```

The module table and the decisions behind it are in [architecture.md](architecture.md). The rules
that keep it maintainable:

- The core never loads a window, GPU or foreign-library system. `make lint` computes the dependency
  closure of `borba-cad` and fails if `cffi`, `cl-opengl`, `cl-freetype2`, `sdl2`,
  `borba-cad/platform` or `borba-cad/bench-gui` appears in it.
- Dependencies point down the module table; `app` never references `platform`.
- Everything specific to SBCL lives in `sbcl-support`.
- World and screen coordinates meet only in `viewport`; pixels never appear in `geometry` or
  `domain`.

## 4. Conventions, and how they are enforced

`make lint` runs after the strict build and fails the gate on any violation.

| Rule | Enforced by |
|---|---|
| Zero `WARNING` and `STYLE-WARNING` on a full rebuild of every system | `make compile` (the build deletes the project's compiled files first) |
| English in code, comments, docstrings, logs and UI text | review (ASCII-only sources are enforced; accents in test data are built with `code-char`) |
| ASCII only, no tabs, no trailing blanks, final newline, lines of at most 120 columns | lint |
| No unfinished-work markers (`TODO`, `FIXME`, `XXX`, `HACK`, "not implemented", "placeholder", "stub") | lint |
| No `ignore-errors`, `read-from-string`, `eval` or `cl:read` on external data | lint |
| A docstring on every exported function, macro, variable, constant and type; for functions and macros one line `NAME: description` per parameter | lint (`sb-introspect`); the lines also become the list in `docs/reference.md` |
| A call to one of the project's own functions with three or more parameters puts each parameter on its own line, aligned under the first (a keyword and its value count as one parameter) | lint, in `src/`, `bench/`, `scripts/` and `test-support/`; test code keeps short fixture calls on one line because the point of such a line is to read inputs and expectation together |
| The core depends on no GUI or foreign-library system | lint |
| Error handling | review, see below |
| Named constants, no magic numbers | review; every constant has a docstring that says what it is and why it has that value |

More conventions, by review:

- One package per module directory (`borba-cad/<module>`), explicit `:export`, `:use #:cl` only and
  package-local nicknames (`dom`, `geo`, ...) for the others.
- Entities are immutable values built by validating constructors. Every document change is a change
  set (`id`, `before`, `after`) applied atomically and invertible: undo is the inverse change set.
- Failure that can be expected is a typed condition from `base/conditions.lisp` (`invalid-entity`,
  `invalid-input`, `layer-error`, `drawing-file-*`, `configuration-error`), raised with a message that
  says what failed, why and what to do, and handled at the application boundary, which shows it in
  the status bar and writes it to the log. Nothing is swallowed.
- Docstrings in the lab format: a one-line description, a blank line, then `PARAM: ...` lines with the
  parameter names in upper case.
- SBCL gotchas met here: `defconstant` with a string or structure value fails on reload (use
  `defparameter` or a function); runtime options such as `--dynamic-space-size` must precede toplevel
  options; `sdl2:with-init` runs its body in another thread, so errors are caught there and
  re-signaled in the caller; `SDL_PushEvent` of a text event crashes `sdl2-compat`, so the event
  translator is tested on locally built event structures; a saved executable must not keep the
  load-time FreeType handle.

## 5. Tests

`make test` runs the FiveAM suites in `tests/`, headless and deterministic (random data comes from a
seeded SplitMix64 generator, never from the clock or the machine). 110,256 checks in about 3 s.

| Suite | Covers |
|---|---|
| `base`, `sbcl-support`, `prng`, `checksum` | Units, tolerances, conditions, configuration; clocks, GC events, files; the generator |
| `geometry` | Every kernel operation, with analytic, brute-force and property-based references |
| `domain-entities`, `domain-drawing` | Entity invariants and protocol; store, drawing, layers, change sets, history, picking, snapping (model-based and property tests) |
| `viewport` | Transforms, zoom, pan, fitting, adaptive grid |
| `persistence` | Exact text of the format, round trips on seeded random drawings, every corruption class with its location, truncation and mutation fuzzing, atomic-save failure injection |
| `observability` | Log format, rotation, sampling, timing, aggregation, GC and memory, the summarizer |
| `scene` | The draw list, culling, clipping, tessellation, selection and preview, determinism |
| `ui`, `app` | Layout, text editing, widgets; scripted input events driving the whole headless application, a randomized event "monkey" test, the demo scenario |
| `bench` | The benchmark harness itself (not the application's performance) |

`make test-gui` runs `platform-tests/` (PNG encoding, SDL event translation, fonts, and a real
window that renders known scenes and reads the pixels back). Without a graphical session the
renderer tests report `Skip` with the reason instead of passing silently. `make test SUITE=name` and
`make test-gui SUITE=name` run one suite.

Rules for tests: no dependence on the screen, the clock or the machine; every discovered bug gets a
named regression test; a persistence or geometry change comes with normal, edge, degenerate and
invalid-input cases and, where one exists, an independent reference.

## 6. Debugging

- `make debug` starts the program with `BORBA_CAD_DEBUG=1`: unexpected errors reach the SBCL
  debugger with a backtrace instead of being logged and ending the process, and debug-level lines go
  to stderr. Use `--log-level trace` for more.
- In `make repl` the whole headless application is available: `(app:make-app (base:default-config))`,
  then `(app:app-handle-event application (app:make-pointer-event :down :left 100d0 100d0))`,
  `(app:app-render application)` and `(scene:draw-list-vertex-count (app:app-draw-list application))`
  show exactly what the window loop would draw.
- Every operation is logged with its duration; `make logs-summary` and `grep` answer "what was
  slow". `docs/performance.md` explains each event.
- A scripted run is the quickest reproduction: add a row to `*demo-script*` in
  `src/app/scenario.lisp` (`:click`, `:drag`, `:key`, `:type`, `:zoom-in`, ...) and run `make demo`.

## 7. Extending the program

### Adding an entity type

The checklist below is derived from the code of the last two types added (text and dimension). Every
step that is forgotten fails loudly at run time or in a test, except the documentation.

1. **Geometry** (`src/geometry/`): the kernel operations the shape needs (distance for picking,
   bounds, measures), exported from `geometry`, with normal, edge, degenerate and invalid-input
   tests and an independent reference.
2. **Domain** (`src/domain/<kind>.lisp`, added to `borba-cad.asd` and exported from `domain`): a
   structure that includes `entity`; a validating constructor that signals `invalid-entity` with a
   reason; an `update-<kind>`; and the methods `entity-kind`, `entity-bounds` (computed once at
   construction), `entity-pick-distance`, `entity-translated`, `entity-snap-points`,
   `entity-properties` (what the inspector edits), `entity-derived-values`, `entity-with-property`.
   Add the keyword to `*entity-kinds*` in `domain/entity.lisp`.
3. **Persistence** (`src/persistence/format.lisp`): one `make-entity-format` row describes the
   fields; the reader and writer are table-driven. A new entity type changes the file format: see
   "Changing the file format" below.
4. **Scene** (`src/scene/scene.lisp`): a clause in the `etypecase` of `emit-entity` (an entity of an
   unknown type is a run-time error, so a forgotten clause is caught by the scene tests).
5. **Application** (`src/app/`): a tool class and its clause in `make-tool` (`tools.lisp`), the
   shortcut in the key table (`input.lisp`) and the toolbar button (`tool-spec` in `panels.lisp`).
6. **Workload** (`test-support/workload.lisp`): a generator for the kind and its weight. This changes
   the checksums of W1 to W3, so the baseline must be recorded again.
7. **Tests**: entity invariants, a round trip and the corruption cases in the persistence suite, the
   scene output, the tool in the application suite, the inspector rows.
8. **Docs**: `docs/geometry.md` (entity table), `README.md`, `CHANGELOG.md`, then `make docs`.

The dimension, the last type added, appears in 14 source files. Three hold the type's own code
(`domain/dimension.lisp`, `geometry/dimension.lisp` and the tool in `app/tools.lisp`); four need a
block of a few lines (the export lists of `domain` and `geometry`, the format row, the scene
clause); seven need one line or a mention in a docstring (the entity kind list, the key table, the
toolbar button, `app` exports, and docstrings in `operations.lisp` and `drawing.lisp`; a comment in the
demo script).

### Adding a tool

A class that specializes the generic functions of `tools.lisp` (`tool-kind`, the pointer, key and
cancel handlers, `tool-preview-entities`), a clause in `make-tool`, a shortcut and a toolbar entry.
A tool only proposes change sets; `app` commits them through the history, which times and logs them.

### Adding a measured operation

Wrap it in `obs:with-operation` (event name such as `geometry.thing`, fields such as
`:entity-count`) or call `obs:record-operation-result`; choose the level (`:info` for user-level
operations, `:debug` for per-interaction ones) so that nothing is written per mouse move or per
frame at the default verbosity. Add the event to the matrix in `docs/performance.md`.

### Adding a benchmark scenario

A function in `bench/scenarios.lisp` that returns a list of measurements (`measure-operation` times
each call with the monotonic clock; its function receives negative indexes while warming up),
registered in `run-workload` in `bench/run.lisp`. Scenarios record; they never assert targets.

### Changing the file format

Every change to what a `.bcad` file may contain bumps `+supported-format-version+` in
`persistence/format.lisp`. Today the loader accepts exactly the supported version and refuses older
and newer ones with a `drawing-file-version-error` that says which version it reads. The first change
must therefore add, before the fields are parsed, an explicit migration from the previous version to
the new one, tested on a file of the old version kept in the tests as a fixture; the specification
requires it, and none exists yet because only version 1 has ever been written.

## 8. Build and packaging

`make build` is `make compile` (strict) plus `make exe`. The executable is an SBCL image saved with
`save-lisp-and-die :executable t :save-runtime-options t`: the heap size (`4GB`) is baked in and every
command-line argument goes to the program. Two details matter: the FreeType library handle that
`cl-freetype2` creates while loading is a dangling pointer in a saved image, so the build clears it
and the program opens its own at startup; and the entry point disables the debugger unless
`BORBA_CAD_DEBUG=1`, so that an unexpected condition ends the process instead of waiting for a
terminal. The executable still needs the native libraries and a font on the machine where it runs;
it finds `var/log` through `BORBA_CAD_ROOT`, or the project directory it was built in.

## 9. Releasing

1. `make check` and `make all` pass from a fresh copy of the repository (`git clone` or `cp -r`
   without `build/` and `var/`).
2. `make bench-baseline` on an idle machine; update `docs/performance.md` from the new reports.
3. `make docs`, `make example`; update `CHANGELOG.md`; set the version in `borba-cad.asd`.
4. The human reviews the acceptance criteria and the experiment report
   ([experiment-report.md](experiment-report.md)) and chooses a license; no `LICENSE` file exists
   until then.
