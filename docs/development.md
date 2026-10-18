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
| Fonts | Noto Sans Mono (`/usr/share/fonts/noto/NotoSansMono-Regular.ttf`), DejaVu Sans Mono, Liberation Mono or FreeMono are found automatically for drawing text; otherwise `--font FILE` or `BORBA_CAD_FONT`. The interface uses Noto Sans Regular and Medium (or DejaVu Sans, Liberation Sans) and falls back to the monospace font when none is installed. A `fonts/` directory beside the executable is searched first. |
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
| `--state-dir DIR` | `BORBA_CAD_STATE_DIR` | Where the program remembers things between runs: the recent files (default `var/state` in the project) |
| `--drawings-dir DIR` | `BORBA_CAD_DRAWINGS_DIR` | Folder where the file dialogs start when no drawing is open (default: the user's documents folder) |
| `--font FILE` | `BORBA_CAD_FONT` | Monospace TrueType font |
| `--language TAG` | `BORBA_CAD_LANGUAGE` | Interface language: `en-US` (the default, always), `pt-BR` or `ru-RU`. The language chosen with the globe button in the title bar is kept in `settings.txt` in the state directory and used next time unless an option or the variable says otherwise |
| `--width N`, `--height N` | | Initial window size in logical pixels (1440 × 900) |
| `--demo`, `--exit-after-demo`, `--screenshot FILE` | | Run the scripted demonstration, close when done, save the last frame as a PNG |
| | `BORBA_CAD_ROOT` | Project root: where `var/log` and `examples` are looked for; defaults to the directory of `borba-cad.asd` |
| | `BORBA_CAD_DEBUG=1` | Do not catch unexpected errors, so a debugger can take them |

Exit status: 0 on a normal exit, 2 for a bad option, 1 for any other failure.

## 3. Layout and layering

```
src/sbcl-support  src/base  src/geometry  src/domain  src/viewport  src/persistence
src/dxf  src/plot  src/image  src/script
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
   reason; an `update-<kind>` that takes every field and the `:id` and `:layer-id`; and the methods
   `entity-kind`, `entity-bounds` (computed once at construction), `entity-pick-distance`,
   `entity-translated`, `entity-snap-points`, `entity-properties` (what the inspector edits),
   `entity-derived-values`, `entity-with-property`, `entity-with-layer`, `entity-with-identity` (paste)
   and `entity-touches-bounds-p` (crossing selection; the default, bounds overlap, is right for filled
   boxes only); `entity-grips` and `entity-with-grip` when the shape has handles; and, in
   `domain/transforms.lisp`, `entity-rotated`, `entity-mirrored` and `entity-scaled`.
   Add the keyword to `*entity-kinds*` in `domain/entity.lisp`.
3. **Persistence** (`src/persistence/format.lisp`): one `make-entity-format` row describes the
   fields; the reader and writer are table-driven. A new entity type changes the file format: see
   "Changing the file format" below.
4. **Scene** (`src/scene/scene.lisp`): a clause in the `etypecase` of `emit-entity` (an entity of an
   unknown type is a run-time error, so a forgotten clause is caught by the scene tests).
5. **Application** (`src/app/`): a tool class and its clause in `make-tool` (`tools.lisp`; a tool that
   takes a fixed number of clicks is a subclass of `point-sequence-tool`), the
   shortcut in the key table (`*tool-shortcuts*` in `tools.lisp`), the draw menu entry (`*draw-menu-tools*` in `menus.lisp`) and
   its icon (`ui/icons.lisp`), the kind's name in `*entity-kind-labels*` (`base/conditions.lisp`)
   and the translations of the new texts (`src/i18n/pt-br.catalog`; the lint lists them).
6. **Workload** (`test-support/workload.lisp`): a generator for the kind and its weight. This changes
   the checksums of W1 to W3, so the baseline must be recorded again. The arc, the ellipse and the
   polygon were left out of the workload on purpose, so that the recorded baselines stay comparable.
7. **Tests**: entity invariants, a round trip and the corruption cases in the persistence suite, the
   scene output, the tool in the application suite, the inspector rows.
8. **DXF** (`src/dxf/`): a clause in the `etypecase` of `write-entity-as-dxf` in `export.lisp` (a forgotten
   one is a run-time error, caught by the export tests), and, when DXF has an equivalent, a clause of
   `import-record` and a kind of shape in `import.lisp`.
9. **Script**: a clause in the `etypecase` of `entity-command` (`script/export.lisp`) so that the drawing can be
   exported as a script, and a command in `script/dsl.lisp` that makes the entity.
10. **Docs**: `docs/geometry.md` (entity table), `README.md`, `CHANGELOG.md`, then `make docs`.

The dimension, the last type added, appears in 14 source files. Three hold the type's own code
(`domain/dimension.lisp`, `geometry/dimension.lisp` and the tool in `app/tools.lisp`); four need a
block of a few lines (the export lists of `domain` and `geometry`, the format row, the scene
clause); seven need one line or a mention in a docstring (the entity kind list, the key table, the
palette entry, `app` exports, and docstrings in `operations.lisp` and `drawing.lisp`; a comment in the
demo script).

### Adding a command to the script language

A function in `src/script/dsl.lisp` with a docstring that says what it does, what it returns and one line
for each parameter (the console's `help` prints it); export it from `src/script/package.lisp`
and add it to `*help-topics*`, which the tests check names only commands that exist and are
documented. Numbers arrive in millimetres and angles in degrees (`angle-radians`); make or change an
entity only through `emit-change` (`add-entity` for a new one), which is what keeps the command one step
of undo and lets a failure take it back, and never call `dom:apply-changes` or a history function
from a command. Anything that touches the window (the selection, the view) goes through the hooks of
the context, so the language stays in the headless core. If the command makes an entity of a new kind,
`drawing-script` in `src/script/export.lisp` needs a clause for it too, and the
round trip test (`random-drawings-survive-a-trip-through-a-script-exactly`) will say when it does.
The messages of a command are Lisp errors in English and are not translated: what the console prints
for a failed command is the message of the condition.

### Translating text

Every text the user reads is written in English and passed through `i18n:tr` (a plain text),
`i18n:trf` (a format control with its values, which the translation may reorder in meaning but
must keep in order and kind) or `i18n:mark` (a text in a table, translated with `tr` where shown).
`invalid-entity-failure`, `layer-failure`, `io-failure` and `path-failure` translate their control
for you. After adding one, run `make lint`: it lists every text with no entry in
`src/i18n/pt-br.catalog` and every entry nothing uses; add the entry, `(ENGLISH TRANSLATION)`, in
UTF-8. `make test SUITE=i18n` checks that each translation asks for the same values as its English
text, that every language translates the same texts, and that no English text reaches the screen in any
language through any of the windows it
walks. Texts that are not for the user (log events, option names, the structure of a damaged
`.bcad`) are not translated. A new language is a catalog file, an entry in `*catalog-files*`
(`src/i18n/i18n.lisp`) and its name in `*translated-languages*` in `scripts/lint.lisp` and in
`*translated-languages-under-test*` in `tests/i18n-tests.lisp`; a catalog with every key of the Portuguese one
is what the lint and the tests ask for. A language whose nouns have more than the two forms that `~[` tells
apart (0 and 1 against the rest) cannot write the plural after the number: Russian, which has three, names
what it counts before the number ("Objects: 3"), which is right for every number, and keeps the directives
of the plural (`~:*~[~;~:;~]`, empty) so that the translation asks for the same values as the English text.
The fonts the program looks for (Noto Sans, DejaVu, Liberation) all have Cyrillic.

### Adding a tool

A class that specializes the generic functions of `tools.lisp` (`tool-kind`, the pointer, key and
cancel handlers, `tool-preview-entities`), a clause in `make-tool`, a shortcut, a palette entry and
an icon.
A tool only proposes change sets; `app` commits them through the history, which times and logs them.

A tool that acts on the selection (rotate, mirror, scale in `src/app/transform-tools.lisp`) is listed in
`*modify-tool-kinds*` instead of the draw list, appears in the Modify menu (`modify-menu-items` in `menus.lisp`) and
is refused by `operation-set-tool` while nothing is selected. A new transformation also needs a generic in
`domain/transforms.lisp` with a method per entity kind, a `*-entities-changes` function in `domain/commands.lisp`
and a console command in `script/dsl.lisp`.

A tool that changes what is clicked rather than the selection (`src/app/reshape-tools.lisp`) works out its change
set in `domain/reshaping.lisp`, which signals `edit-refused` with a sentence when it cannot be done, and commits it
with `commit-reshaping`, which says the sentence in the status bar. It is in `*modify-tool-kinds*` but not in
`*selection-tool-kinds*`, so it needs no selection.

A tool that only measures (`src/app/measure-tools.lisp`) never commits a change: it keeps its points, draws them
and a text label through `tool-preview-entities` (a text entity sized in pixels, so the label does not change
with the zoom), and says the result in the status bar. It is listed in `*measure-tool-kinds*` and appears in the
Measure menu.

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

A change to what a `.bcad` file may contain is a new format version, and the version keeps reading
the old ones. In `persistence/format.lisp`: raise `+supported-format-version+`; give a new field the
`:since` of the new version and the `:default` that an older file gets for it, and a new entity kind
the `:since` of the new version. The loader then reads an older file as the newer one with the
defaults in place, refuses in it what its version does not have, and the writer writes the newest
version. Keep the fixture of the old version in `tests/fixtures/` (version 1 is
`v1-floor-plan.bcad`, kept as it was written) and add a test to `tests/migration-tests.lisp` that
reads it, saves it and reads it back. A migration that a default cannot express (a field that changes
meaning) is a function from the parsed fields of the old version to those of the new one, applied in
`parse-named-fields` or `parse-entity` for files older than the change; it too gets a fixture.

## 8. Build and packaging

`make build` is `make compile` (strict) plus `make exe`. The executable is an SBCL image saved with
`save-lisp-and-die :executable t :save-runtime-options t`: the heap size (`4GB`) is baked in and every
command-line argument goes to the program. Two details matter: the FreeType library handle that
`cl-freetype2` creates while loading is a dangling pointer in a saved image, so the build clears it
and the program opens its own at startup; and the entry point disables the debugger unless
`BORBA_CAD_DEBUG=1`, so that an unexpected condition ends the process instead of waiting for a
terminal. The executable still needs the native libraries and a font on the machine where it runs
(the distribution below carries them). Run as the saved executable the program is *installed*: its
folder is the project root (so `examples/` and `fonts/` beside it are found), and its log and its
list of recent drawings go to the user's state directory (`$XDG_STATE_HOME/borba-cad`, by default
`~/.local/state/borba-cad`; `%LOCALAPPDATA%\BorbaCAD` on Windows) because the folder may not be
writable. Run from the source, it keeps using `var/` in the project.

### Distribution for Linux: `make dist`

`make dist` builds the executable and then `dist/borba-cad-VERSION-linux-x86_64/` and its `.tar.gz`
(22 MB). The folder runs from a double click:

| File | What it is |
|---|---|
| `Borba CAD.desktop`, `borba-cad` | What to double-click: the first is a desktop launcher that works from wherever the folder is, the second the shell script it starts. The launcher makes `lib/` visible and, when the program cannot start, shows why in a dialog (`kdialog`, `zenity`, `notify-send` or `xmessage`, whichever exists) and saves it to `last-error.txt` |
| `borba-cad.bin` | The program: the executable of `make exe` |
| `lib/` | What the program loads and a desktop may lack: SDL2 (the SDL3 compatibility layer) and SDL3, FreeType and its libraries, Zstandard. Found with `ldd`, recursively; the C library and everything that belongs to the graphics stack (OpenGL, X11, Wayland) are never copied |
| `fonts/`, `examples/`, `LICENSES/`, `borba-cad.svg` | Noto Sans (regular, medium) and Noto Sans Mono, the sample drawing, the licenses of the bundled parts, the icon |
| `install.sh`, `uninstall.sh` | Per-user install: the applications menu with the icon, and a double click on a `.bcad` file opens it. Nothing outside the home folder is touched |
| `BUILD-INFO.txt` | Version, commit, build machine, glibc, SBCL, bundled libraries |

`make dist-check` starts the folder in a clean environment (`env -i`, so nothing of the developer's
shell leaks in) and checks that the scripted demonstration draws and saves a valid PNG; that every
library the folder needs, except the C library and the graphics stack, resolves inside the folder
(`ldd` with only `lib/` on the library path); that a running program has loaded SDL from the folder;
and that it opened the fonts of the folder (the program logs them in `app.start`). One thing it
reports and does not refuse: when the desktop's own libraries (client-side window decorations) load
the system FreeType first, that copy serves, and the bundled one is for systems that have none.

Limits: the folder is built on the machine that runs `make dist`, so it needs a C library at least
as new as that machine's (the build machine and its glibc are in `BUILD-INFO.txt`); to run on older
distributions, build on the oldest one you want to support. There is no AppImage. The program has no
license file until the author chooses one, so the folder carries only the licenses of its parts.

### Windows

**The Windows build has been made by GitHub's Windows machine (the fourth run of the workflow, 2026-10-15),
but nothing has been run on Windows.** The first three runs found what was missing: the C compiler that
cl-freetype2 calls is a Windows program, so the include folders are given to it in Windows form (`CPATH`,
with the `freetype2` folder and its `freetype` subfolder), and a Windows checkout turned the line ends into
CRLF, which broke a format control and would break every shell script (`.gitattributes` asks for LF). What
exists, and how far it was tried:

| Piece | State |
|---|---|
| `scripts/dist-windows.sh`, `make dist-windows` | Written for an MSYS2 MINGW64 shell. Its one delicate step, turning the console program into a graphical one by writing 2 over the Subsystem field of the PE header, is tested on a fabricated header (`tests/packaging-tests.lisp`). The DLL collection and the zip are unproven |
| `.github/workflows/dist.yml` | A Linux job (Arch container, the whole `make check`, then `dist` and `dist-check` under `xvfb`: it passes), a Windows job (MSYS2 with SBCL, SDL2, FreeType and gcc: it builds and packages, and does not run the program) and a job that publishes the Windows zip as a release |
| `src/sbcl-support/` | The POSIX calls (`sb-posix`: stat, access, chmod, folder listing, rename, fsync) are written for Linux and guarded with `#-win32`; on Windows `portable-filesystem.lisp` (standard Common Lisp and UIOP) answers instead. The tests hold the portable versions to the same answers as the POSIX ones on Linux, but the Windows branches were never compiled. The monotonic clock falls back to the coarser `get-internal-real-time`, the memory figure that reads `/proc` is reported as unavailable, and the atomic rename becomes delete-then-rename |
| Fonts | The bundled Noto fonts, else Segoe UI and Consolas from `C:\Windows\Fonts` |
| Not looked at | Whether `cl-sdl2` and `cl-freetype2` build under MSYS2's SBCL (`cl-freetype2` needs a C compiler and the FreeType headers when first loaded), whether `sdl2:with-init` and OpenGL 3.3 context creation behave the same, DPI scaling, the file dialog on drive letters and backslashes (`typed-path` handles `C:\...`, the tests only exercise Unix paths), and the console-less program's standard streams |

The Windows job of the workflow ends by publishing the `.zip` as a release (`build-N` when the run was
started by hand, the tag when it was started by a tag), whose file can be downloaded by anyone with the link,
without a GitHub account; the artifact of the run needs a login. To try it on a Windows machine: install MSYS2, open its MINGW64 shell, `pacman -S make git zip curl
mingw-w64-x86_64-gcc mingw-w64-x86_64-sbcl mingw-w64-x86_64-SDL2 mingw-w64-x86_64-freetype`, install
Quicklisp, then `make dist-windows` in the repository. `dist/borba-cad-VERSION-windows-x86_64/`
holds `borba-cad.exe` (no console window) and `borba-cad-console.exe` (with one, to see what the
program prints when the first does not start).

## 9. Releasing

1. `make check` and `make all` pass from a fresh copy of the repository (`git clone` or `cp -r`
   without `build/` and `var/`).
2. `make bench-baseline` on an idle machine; update `docs/performance.md` from the new reports.
3. `make docs`, `make example`; update `CHANGELOG.md`; set the version in `borba-cad.asd`.
4. The human reviews the acceptance criteria and the experiment report
   ([experiment-report.md](experiment-report.md)) and chooses a license; no `LICENSE` file exists
   until then.
