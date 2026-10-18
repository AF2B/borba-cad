# Borba CAD — Architecture

| | |
|---|---|
| Status | Accepted for implementation (human instruction of 2026-10-13: proceed to v1 without further gates) |
| Inputs | [specification.md](specification.md) (approved, all recommended defaults), spike evidence in §12 |

## 1. Principles

- The core is **headless**: domain, geometry, persistence, observability, scene building, UI logic and application logic load and test with no window, GPU or foreign library (NFR-2).
- Only `borba-cad/platform` touches SDL, OpenGL and FreeType. It consumes a plain data structure (the *draw list*) and produces plain data (*input events*).
- Data first: entities are immutable values; every document change is an explicit, invertible *change set*.
- Measure before optimizing. The first renderer is simple; optimization happens only against a baseline.

## 2. Systems and packages

One ASDF file (`borba-cad.asd`), seven systems. One package per module directory, named `borba-cad/<module>`, with explicit exports.

| System | Packages (directory under `src/`) | Responsibility | Depends on |
|---|---|---|---|
| `borba-cad` (core) | `sbcl-support` | Everything SBCL-specific: monotonic clock, wall clock, heap/RSS, GC-event queue, environment, `fsync`/`rename`, the timeout and the uninterruptible section, the output stream that keeps only its first characters. | SBCL contribs `sb-posix` |
| | `i18n` | The interface languages: `tr`, `trf`, `mark`, the catalogs (data files read at load time). | none |
| | `base` | Units, tolerances, conditions, configuration. | `sbcl-support`, `i18n` |
| | `geometry` | Bounds, distances, clipping, dimension layout, text extents, contour area and angles. Doubles only; no screen types. | `base` |
| | `domain` | Entities, layers, drawing, change sets, history, picking, snapping, transformations (rotate, mirror, scale), inspector property model. | `geometry`, `base` |
| | `viewport` | World↔screen transform, zoom/pan, extents, adaptive grid. | `geometry`, `base` |
| | `persistence` | `.bcad` reader/writer/validator, atomic save, path cleaning, the recent-files list, the settings. | `domain`, `base` |
| | `plot` | Plotting on a sheet: the sheet sizes, the scale and the placement, the entities flattened into strokes, filled triangles and text in sheet millimetres, and the writers of a PDF (by hand) and of an SVG. | `domain`, `geometry`, `base` |
| | `dxf` | ASCII DXF: the reader (groups, records, sections), the import into a drawing with a report of what was left out, the R12 writer, the color index. | `persistence`, `domain`, `geometry`, `base` |
| | `image` | Picture files made from pixels: the JPEG encoder. | none |
| | `script` | The language the program is driven with: the commands (Lisp functions in `borba-cad/script`, read in the user package `borba-cad/user`), the context and transaction a command runs in, the evaluator (reads, runs, times out, reports), and the writer that turns a drawing into the commands that draw it. | `persistence`, `domain`, `geometry`, `base`, `i18n`, `sbcl-support` |
| | `observability` | Structured log, rotation, timers, aggregators, GC/memory sampling, perf snapshot, log summary. | `sbcl-support`, `base` |
| | `scene` | Draw list and the scene builder (drawing + viewport + selection + preview → draw list). | `domain`, `viewport`, `geometry` |
| | `ui` | Window layout; widgets drawn into the draw list (buttons, chips, cards, rows, fields, tooltips); vector icons and the brand mark; visual tokens; text measuring, fitting and wrapping; text fields. | `scene`, `base` |
| | `app` | Application state, input-event handling, tools, commands, file operations, the console, scripted scenarios. | all of the above |
| `borba-cad/platform` | `platform` | SDL window + GL context, GL renderer, font set and glyph atlas (three fonts), text measurer, event translation, main loop, CLI, screenshot. | core, `sdl2`, `cl-opengl`, `cl-freetype2` |
| `borba-cad/test-support` | `test-support` | Seeded PRNG, deterministic workload generator. | core |
| `borba-cad/tests` | `tests` | Correctness suites (FiveAM), headless. | core, test-support, bench (for the harness tests), `fiveam` |
| `borba-cad/platform-tests` | `platform-tests` | Tests of the window program: PNG, SDL event translation, fonts, the window loop's logic and, with a display, the renderer on real pixels. | platform, test-support, `fiveam` |
| `borba-cad/bench` | `bench` | Headless benchmarks: scenarios, report writer, benchmark log. | core, test-support |
| `borba-cad/bench-gui` | `bench-gui` | Windowed frame benchmark with the real renderer. | bench, platform |

Dependency rule: arrows only point down the table. `app` never references `platform`; the loop in `platform` calls `app`.

## 3. Dependencies (verified 2026-10-13)

| Dependency | Used by | Dist version | License | Last upstream commit | Why / risk |
|---|---|---|---|---|---|
| `sdl2` (cl-sdl2) | platform | 20231021 | MIT | 2025-07-06 | Window, GL context, events. Runs over `sdl2-compat` 2.32.74 (SDL3 3.4.18). |
| `cl-opengl` | platform | 20260101 | BSD | 2026-06-26 | OpenGL 3.3 core bindings. |
| `cl-freetype2` | platform | 20241012 | BSD-3-Clause | 2024-02-10 | Glyph rasterization. Low upstream activity but stable; works with FreeType 2.14.3. Needs `gcc` and FreeType headers on first load (cffi-grovel). |
| `fiveam` | tests | 20241012 | BSD-3-Clause | 2024-09-27 | Test framework (decision D4). |
| Transitive | platform / tests | — | MIT / BSD / public domain | — | `cffi`, `cl-autowrap`, `cl-plus-c`, `alexandria`, `trivial-garbage`, `trivial-features`, `bordeaux-threads`, `trivial-channels`, `trivial-backtrace`, `asdf-flv`. |

Native libraries: `libSDL2` (sdl2-compat), `libGL` (libglvnd/NVIDIA), `libfreetype`. SBCL contribs: `sb-posix` (core), `sb-introspect` (lint and the API reference generator only). The core has **no** third-party Lisp dependency. Dependencies are acquired from Quicklisp dist `2026-01-01`, pinned and verified by `make check-env` (decision D5).

## 4. Key decisions

| # | Decision | Why |
|---|---|---|
| A1 | Entities are immutable `defstruct` values with validating constructors; raw constructors are private. | Invariants hold by construction; undo is trivial; no aliasing bugs. |
| A2 | Mutations are **change sets**: lists of `(id, before, after)`. Undo = apply the inverse. | One uniform mechanism for create, delete, move, edit (decision D1). No snapshots, bounded memory. |
| A3 | Entity ids increase monotonically and are never reused; stacking order = id order. | Undo of a delete restores the original stacking position by id alone. |
| A4 | CLOS generic functions for entity behavior (`entity-bounds`, `entity-pick-distance`, …) and for tools. | Open dispatch over six types and seven tools; adding a type is a documented checklist (H6). Closed sets (input events) use `etypecase`. |
| A5 | Canonical unit: millimetre, `double-float` (decision D3). Display unit (mm/cm/m) is a drawing setting converted only in `base/units`. | One conversion site; integers such as 150 mm are exact. |
| A6 | Text uses a **fixed-advance** layout: advance = `+text-advance-ratio+` × height (0.6 em). Domain bounds and rendering share the model. | Verified: Noto Sans Mono advances exactly 0.6 em (S3). Keeps `domain` independent of fonts and text bounds exact. |
| A7 | The **draw list** is the renderer boundary: one `single-float` vertex array (x y u v r g b a) plus ordered commands (`:lines`, `:triangles`, `:text`). Tests read the draw list directly. | No recording-renderer duplicate; GPU-free integration tests. |
| A8 | Screen space is logical pixels; the GL backend rasterizes text at logical size × display scale. | Fractional scaling handled in one place. |
| A9 | Single-threaded main loop (SDL main thread). The GC hook runs in SBCL's `finalizer` thread and only does a lock-free push; the main thread drains it (S2). | No locks in the hot path; no logging from the hook. |
| A10 | Persistence uses a small custom S-expression reader, not `cl:read`. | No code evaluation, no symbol interning, line/column errors, bounded exponents (NFR-7). |
| A11 | No spatial index and no incremental redraw until the baseline demands them (spec §8). Pick and window selection scan with a bounds pre-filter. | Avoids speculative complexity; measured in W0–W3. |
| A12 | SBCL-specific code lives in `sbcl-support` only. | One place to port or replace. |
| A13 | Float traps stay enabled in Lisp code. S1 showed no trap in SDL/GL calls with defaults, so no masking is needed; the platform masks only if a trap is observed. | Keeps NaN/infinity detection in geometry. |
| A14 | Layer deletion (only allowed when the layer is empty) clears undo history. Other layer edits are immediate and not undoable. | Prevents dangling layer references from undo; keeps D1 scope to entity edits. |
| A15 | The demonstration is data: `*demo-script*` (rows such as `(:click 0 0)`) interpreted by `run-scenario-step`. The same script drives the demo, the tests and the shipped `examples/floor-plan.bcad`. | One definition; the example is always what the program itself draws. |
| A17 | Interface text (`:ui`, `:ui-strong`) is proportional (Noto Sans) and measured through `ui:*text-measurer*`. The window program installs a measurer that reads the glyph advances of the very fonts that draw, so layout and drawing agree to the pixel; headless code and tests keep the fixed 0.6 em default. Drawing text (`:mono`) stays on the fixed grid of A6. | Truncation, centering and caret placement need real widths; geometry must not depend on a font. A window test checks that no ink lies beyond the measured width. |
| A18 | The look of the interface is data: colors in the theme, strokes and opacities in `ui/tokens.lisp`, icons and the brand mark as shape lists on a 24-unit grid, everything built from triangles by `scene/shapes.lisp`. No image files. | One place to tune; crisp at any size or display scale; nothing to ship or load. |
| A19 | The file dialog is state plus widgets, like every panel: `file-browser.lisp` holds the dialog's state and everything it can do without a window (list, sort, filter, navigate, select, create a folder, resolve typed names), `file-dialog.lisp` its actions and key handling, `file-dialog-widgets.lisp` the widgets rebuilt from the state each frame. Folder listing is an SBCL-specific primitive in `sbcl-support`; recent files are a plain text file in the state directory (`persistence/recent.lisp`) that can fail without harm. Tests never touch the user's folders: their configuration has no state directory and starts dialogs in a private scratch folder. | The dialog is testable with scripted events and temporary folders, and its own suite drives it end to end; the random-event test is kept from wandering into the real home folder. |
| A16 | The window loop is a `session` plus `step-session`: everything between two batches of SDL events is a plain function of the session, testable without a window. | The scripted demo once stalled when a window received no event; the fix needed a regression test that does not open a window. |
| A20 | The interface is translated by the English text itself: a message is written in English where it is used and passed through `i18n:tr` (or `trf` for a format control, `mark` for a text in a table); a language is a catalog, a UTF-8 data file mapping English texts to translations, read when the system loads. English (US) needs no catalog and is the default whatever the system locale says; a text without a translation shows in English. The headless core translates what it reports (property names, error reasons) through the same functions, so the language is one dynamic variable, `i18n:*language*`, set by the application. The choice is kept in `settings.txt` in the state directory. | No key table to keep in step with the code, nothing to look up at the point of use, a translator edits data and not code, and the lint (`translation-violations`) fails when the code and the catalog disagree, in either direction. Format directives of a translation are checked against the English text by a test. Pluralization uses the format directive `~[`, so each language writes its own forms. |
| A21 | DXF is a module of its own that depends on the domain and never the other way round, in three steps that can each be tested alone: the reader turns the text into records (it evaluates nothing and bounds every size), the importer turns records into *shapes* in the units of the file (blocks expanded there, with affine transforms) and only then, when the extent of everything is known, into entities of the drawing, so that a drawing whose coordinates are out of range can be moved as a whole; and the writer writes version R12, the oldest that every program reads. Everything that cannot be carried over is counted and said in a report, never dropped quietly. | The format is a moving target the program does not own; keeping it at the edge, with a report, keeps the domain clean and the user informed. R12 has no ellipse or lightweight polyline and no required handles, so a file that cannot be rejected by a strict reader is easy to write; the cost is that an ellipse, a polygon and a dimension leave as polylines, lines and text. |
| A22 | The program is driven by a language that is Lisp itself, not a dialect of its own: the commands are ordinary functions in the package `borba-cad/script`, and what the user types is read in `borba-cad/user`, which has Common Lisp and those commands and nothing of the program's internals. A command acts on the drawing at once, so the step after it sees it, and keeps the net change of each entity (before and after, for the first and the last time the command touched it); the application records those as one step of undo (`history-record`, which takes changes already applied) and, when a step fails, the time runs out or the limit of changes is reached, applies their inverse. Applying one change is uninterruptible (`sbcl:with-uninterrupted`), so a timeout cannot stop a change half way; what a command prints goes through a stream that keeps only the first 200,000 characters. A script file is run only after the user chose it and answered a question that says it is a program, and it has its own, longer limit. Reading never evaluates (`*read-eval*` is off) and a drawing file or the clipboard is never a script. | A restricted dialect was considered and rejected: a sandbox made of a list of allowed symbols gives a false feeling of safety and a language that is not Lisp, and the console runs on the user's own machine with the user's permissions, so what protects them is that nothing runs unasked, that a failed command leaves nothing behind and that a runaway one stops. The cost is that the command is not isolated: a command can do what Lisp can, including things that are not undone (layers, files). Layers and the drawing's settings are outside the step of undo, as they are in the layers panel. |
| A23 | A picture of the canvas is made in two halves. The window program, which is the one that has the pixels, draws the scene again on its own (the grid and the drawing, with no selection, tool, cursor or panels, at the view of the window) and reads back the canvas rectangle at the resolution of the display; it hands the pixels to the application through a function the application holds (`image-capturer`), the way the clipboard is handed over. The application encodes the pixels (the encoder is in the core, in `image`), saves them atomically and says how it went. | The core stays free of OpenGL and SDL and the whole export, but the reading of the pixels, is tested without a window, with a function that returns a picture made for the test. Drawing the scene again, instead of reading what is on the screen, leaves out what a picture should not have (the selection, a menu or the dialog that is open) and needs no frame to be on the screen. The encoder is ours because a JPEG writer is small and the project avoids a dependency for it; the cost is that it writes only baseline JPEG. |

## 5. Data structures

- **Geometry**: `bounds` (min-x min-y max-x max-y). Kernel functions take raw doubles.
- **Entities** (`domain`): base `entity` (`id`, `layer-id`) included by `line-entity` (x1 y1 x2 y2), `rectangle-entity` (min-x min-y width height), `circle-entity` (center-x center-y radius), `polyline-entity` (flat `(simple-array double-float (*))` of x y pairs, `closed`), `text-entity` (x y string height), `dimension-entity` (x1 y1 x2 y2 offset text-height unit precision; its text and layout are computed once). Flat unboxed storage keeps the GC heap small (H2 lever); about 260 bytes per entity are retained (performance.md §3).
- **Layer**: `id`, `name`, `color` (RGB bytes), `visible`, `locked`.
- **Drawing**: layers (ordered vector + id table), entity store (id table + id-sorted vector), `current-layer-id`, grid settings, unit and display unit, default text height, `next-entity-id`, `next-layer-id`, `revision` (bumped on every mutation).
- **Session / history** (`app`): undo and redo stacks of change sets (depth `+history-depth-limit+`), file path, saved revision (dirty = revision ≠ saved revision).
- **Viewport**: world point at the canvas center + scale (pixels per mm) + logical canvas size + the window position of the canvas's top-left corner (`origin-x`, `origin-y`, so every coordinate is a window coordinate). Screen = origin + canvas size / 2 + (world − center) × scale, with y flipped; the center-relative form avoids cancellation at large coordinates.
- **Draw list**: described in §7.

## 6. Geometry (details in `geometry.md`)

World space is right-handed with +Y up; angles are counter-clockwise radians. Kernel: point–segment distance and closest point, point–circle-outline distance, point–rectangle-outline distance, bounds algebra, Liang–Barsky segment clipping (so no far-off coordinate ever reaches single-float vertices), aligned-dimension layout (extension lines, dimension line, arrowheads, text anchor and angle), text extents. Tolerances are centralized in `base/tolerances.lisp` with written rationale; pixel-based pick and snap radii live in `app`.

## 7. Rendering

1. `scene:build-scene` walks visible entities (layer visible, bounds intersect the culled view), emits lines/triangles/text runs into a reused draw list (no per-frame consing in steady state), then grid, selection highlight, tool preview, snap marker.
2. `ui` appends panel chrome, widgets and the performance overlay to the same draw list.
3. `platform` uploads the vertex array once per frame (`glBufferSubData` through a pinned Lisp vector) and replays the commands with one shader: `color × texture(atlas, uv).r`. Solid shapes sample a white texel; glyphs sample the atlas. Lines are `GL_LINES` (1 px) with 4× MSAA.
4. Circles are tessellated by on-screen radius (sagitta ≤ `+circle-chordal-error-pixels+`, segment count clamped to `[+circle-minimum-segments+, +circle-maximum-segments+]`). Entities smaller than one pixel draw as a 1-px dash.
5. Text: glyph bitmaps are rasterized by FreeType per font and pixel size into a shelf-packed R8 atlas (sizes ≤ 32 px exact, larger sizes rounded up to a ×1.25 ladder), cached by (font, size, code point). Runs support rotation (aligned dimensions). Interface text is placed glyph by glyph on whole framebuffer pixels so that it stays crisp.
6. The first renderer rebuilds the draw list whenever the scene is dirty (pan/zoom included). The baseline shows the CPU bottleneck the plan anticipated: with 100,000 entities all in view a frame takes about 30 ms, almost all of it in the scene build, while the GPU wait is under 1 ms (performance.md §4 and §6). The planned optimization, world-space retained buffers with a GPU view transform or a spatial index, is not done: it must start from a profile and be measured before and after (spec §6.4).

## 8. Input and application model

- Events (`app`): `pointer-event` (move/down/up, button, x, y, modifiers), `wheel-event`, `key-event`, `text-event`, `resize-event`, `quit-event`. `platform` translates SDL events; tests build events directly. The window loop (`platform/main.lisp`) is a `session` and `step-session` (decision A16). Synthetic SDL text events are **not** pushed through SDL (S1: `SDL_PushEvent` of text input crashes sdl2-compat); the translator is tested on locally built event structs.
- `handle-event` routes: modal prompt → UI widgets → canvas (pan/zoom → active tool). Cursor world position passes through snapping: object snap (endpoint, midpoint, center, intersection, quadrant, perpendicular, tangent; radius in pixels) → ortho/polar → grid snap → raw.
- Tools (CLOS): select, line, rectangle, circle, polyline, text, dimension. Each exposes pointer-down/move/up, key, cancel and preview. Tools only *propose* change sets; `app` commits them to history.
- Move-by-drag renders a preview offset and commits one change set on release (one undo step).
- Errors are conditions (`borba-cad/base`): `invalid-entity`, `invalid-input`, `drawing-file-error`, `layer-error`, `configuration-error`. `app` converts them into status messages and log entries; nothing is swallowed.

## 9. Persistence (`.bcad`, format version 5)

UTF-8 text, one S-expression. Numbers are decimal or exponent doubles printed shortest-round-trip, so doubles survive bit-exactly. Strings use `\"` and `\\` escapes. Keywords are bare words.

```lisp
(drawing
 (format-version 5)
 (unit millimeter)
 (display-unit meter)
 (grid (spacing 100.0) (visible t) (snap t) (object-snap t))
 (current-layer 0)
 (next-layer-id 2)
 (next-entity-id 4)
 (layers
  (layer (id 0) (name "0") (color 235 235 235) (visible t) (locked nil))
  (layer (id 1) (name "walls") (color 255 120 80) (visible t) (locked nil)))
 (entities
  (line (id 1) (layer 1) (x1 0.0) (y1 0.0) (x2 5000.0) (y2 0.0))
  (text (id 3) (layer 0) (x 100.0) (y 100.0) (height 250.0) (string "Living room") (angle 0.0))))
```

The reader rejects unknown keys and entity types, wrong arity or types, non-finite or out-of-range numbers, duplicate ids, dangling layer references, a missing default layer, ids at or above `next-*-id`, and a `format-version` that is newer than the program or older than the oldest it reads, each with line and column. A dimension stores its `unit` and `precision` next to its geometry. Exponents are bounded before conversion. `save-drawing` writes a sibling temporary file, flushes and `fsync`s it, then renames over the target; the temporary file is removed on failure. Versions and migration: the program writes the newest version (5) and reads every version from the oldest it supports (1). What a version added is recorded in the format table (`persistence/format.lisp`) as the `since` of a field or of an entity kind, with the `default` that a field gets when the file is older than the field; that table is the migration. A version 1 file is read with the fields that came later filled in with their defaults (a text gets the angle 0), is not allowed to contain them, and is written back as version 5. Version 2 added the angle of a text, the arc, the ellipse and the polygon; version 3 added the radius, diameter and angle dimensions; version 4 added the hatch; version 5 added the `(blocks ...)` section, between the layers and the entities, and the `insert` entity, whose `block` field names a block of that section. A newer file is refused with `drawing-file-version-error`, which names the version this program reads. The fixtures `tests/fixtures/v1-floor-plan.bcad`, `v2-plan.bcad`, `v3-plan.bcad` and `v4-plan.bcad` are files of versions 1 to 4 kept as they were written, and the migration suites read them (development.md §7).

## 10. Observability

- **Log line**: `2026-10-13T21:00:00.123Z INFO geometry.pick duration_ms=0.412 entity_count=1500 result=success`. Wall-clock timestamp (UTC, ms), severity, event name, `key=value` pairs; values with spaces are quoted. Durations come from the monotonic clock (`clock-gettime`, ~16 ns per read, S2).
- **Sinks**: rotating file under `var/log/` (default 5 files × 5 MiB, rotated by size; constants in `base/config`), optional stderr. One flush per entry.
- **Levels** `error warn info debug trace`; per-event sampling (log 1 of N) for high-frequency events. Per-interval aggregators emit one entry per interval, named like the event (count, mean, p95, max), for `render.frame`, `input.event` (per event type) and `viewport.update`. Mouse moves are never logged individually, and at the default level neither are redraws or picks (those are debug-level). Benchmarks log one `bench.run` entry per scenario to `benchmark.log` next to the program's log.
- **GC**: `sb-ext:*after-gc-hooks*` pushes `(timestamp, delta of sb-ext:*gc-run-time*)` onto a lock-free list; the main loop drains it into `gc.event`. `*gc-run-time*` has µs resolution and matched an independent stall detector (86.3 ms vs 85.9 ms); `*gc-real-time*` has only ms resolution and is not used.
- **Memory**: `sb-kernel:dynamic-usage`, `sb-ext:get-bytes-consed`, RSS from `/proc/self/statm` (≈3.6 µs per read), sampled every `+memory-sample-interval-seconds+`.
- **Overlay**: `telemetry-overlay-lines` (rolling frame stats, last operation durations, entity counts, GC, memory) drawn by `ui` on F3 or the Perf button; its measured cost is in performance.md §8.
- **Headless inspection**: `make logs-tail`, `make logs-summary` (`observability` log parser; count/total/p50/p95/max per event).

## 11. Testing, benchmarks, build

- **Tests**: FiveAM suites mirror the packages (`tests/`, headless) and the window program (`platform-tests/`). Seeded SplitMix64 PRNG (own implementation, stable across SBCL versions) feeds the workload generator and randomized round-trip tests. Checks that need a display report `Skip` with the reason when there is none.
- **Static gates** (`make compile` and `make lint`): zero WARNING/STYLE-WARNING on forced rebuild of every system; every exported symbol documented with each parameter named in its docstring (`sb-introspect`); ASCII-only sources, no tabs or trailing blanks, lines of at most 120 columns; no placeholder markers; no `ignore-errors`, `read-from-string`, `eval` or `cl:read`; one parameter per line in calls to the project's functions with three or more parameters (production code); the core's dependency closure contains no GUI/CFFI system.
- **Benchmarks**: `make bench` runs W0–W3 headless (scene, pick, window selection, snap, edits, persistence, memory soak, instrumentation overhead with a control); `make bench-gui` adds real-GL frame timing with the GPU wait. Reports go to `bench/reports/` with an environment fingerprint and workload checksums; `make bench-baseline` records them in `bench/reports/baseline/`.
- **Build**: ASDF, with fasls redirected to `build/fasl/`; `make compile` is the warning-free build of every system, `make build` adds the executable `build/borba-cad` (`save-lisp-and-die`, `:save-runtime-options t`); `make check` runs the whole gate.

## 12. Spike evidence (2026-10-13, SBCL 2.6.9, Wayland, RTX 4060)

| Spike | Result |
|---|---|
| S1 window/GPU/input | `sdl2` + `cl-opengl` open a Wayland window with a GL 3.3 core context (NVIDIA 615.71.09) over sdl2-compat. No float trap with default traps. 10,000 line segments rebuilt and uploaded every frame: CPU time p50 0.42 ms, p95 0.57 ms, max 78 ms (cause not yet attributed); swap with vsync p50 12.8 ms; frame interval p50 13.2 ms. UTF-8 text decoding by cl-sdl2 is correct. `SDL_PushEvent` of a wheel event works; of a text event it crashes sdl2-compat. `sdl2:with-init` runs its body on a dedicated "SDL2 Main Thread". |
| S2 metrics | `clock-gettime` monotonic: 16 ns per read. `get-internal-real-time` ticks at ~1 ms (not used). GC hook fires in the `finalizer` thread; `*gc-run-time*` (µs) tracks real stalls; with ~110 MiB live (1.5 M objects) pauses reached 48–86 ms, with a small live set 1–2 ms. Default heap here is 1 GiB. |
| S3 text | FreeType + GL atlas renders accented Latin glyphs correctly. Per-size rasterization is crisp; scaling one bitmap blurs. 100 glyphs rasterized and uploaded in 3.1 ms. Noto Sans Mono advance is exactly 0.6 em. |
| S4 packaging | A 64 MB executable (no compression) reopens SDL, GL and FreeType at startup; first frame 79–95 ms after `main`. cl-freetype2's load-time `*library*` is a dangling pointer after `save-lisp-and-die`: the app creates its own handle at startup and the build clears `*library*`. `:save-runtime-options t` keeps the heap size and passes every CLI argument to `main`. |

Spike code is archived in `spikes/` (not part of any system); its output is git-ignored.

## 13. Deviations from the specification

The specification stays as approved; these are the places where the implementation differs from it or
makes it more precise.

| Topic | Specification | Implementation |
|---|---|---|
| Selection and geometry events | `selection.pick`, `selection.window` | `geometry.pick`, `geometry.window-select`; also `entity.edit`, `history.undo`, `history.redo` |
| Dimension | two points, offset, text height | also `unit` and `precision`, so its text does not change with the display unit |
| Polyline in the inspector | every vertex editable | the first 6 vertices are listed for numeric editing (`+inspector-maximum-vertex-rows+`) |
| Viewport | center, scale, size | also the window position of the canvas, so that all coordinates are window coordinates |
| File load phases | read, parse, validate, build | read and parse; parsing includes validation and building |
| GC pause | "pause" | CPU time of the collector per collection (`*gc-run-time*`), which equals the stop time with one working thread |
| Frame GPU time | measured only with reliable timer queries | not measured with a vendor timer; the windowed benchmark reports the `glFinish` wait instead |
| `make build` | strict build and executable | `make compile` is the strict build, `make build` adds the executable; `make check` is the whole gate |
| `make docs` | tool chosen in Design | a Markdown reference generated from the docstrings with `sb-introspect` |
| Migrations | explicit, tested migration for every format change | none yet: version 1 is the only version; the loader refuses any other |
| Spikes | S1 to S4 before Design | done; evidence in §12 |

