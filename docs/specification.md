# Borba CAD — Specification

| | |
|---|---|
| Status | **APPROVED (v1.0) — human approval of 2026-10-13; all recommended defaults D1–D10 accepted; spikes authorized; proceed to v1 without further gates** |
| Implementation | v1 implemented. The text below is the approved specification and is kept as approved; where the implementation refines or differs from it, [architecture.md](architecture.md) §13 lists each case. |
| Date | 2026-10-13 |
| Workflow | Phase 2 (Specification) and Phase 3 (Review). Design and implementation stay blocked until this is approved. |
| Project root | `/home/borba/Documents/borba-cad` |

The requested path `/documents/borba-cad` does not exist and `/` is not writable without root. `~/Documents` is the existing, writable equivalent, confirmed by the human on 2026-10-13. The root is a configuration value, never hardcoded in application code.

## 1. Intention

### 1.1 Purpose

Borba CAD is an engineering experiment: can Common Lisp (SBCL) be the foundation of a professional civil-engineering software ecosystem? The MVP is a native 2D drafting application, but its real product is **evidence**: tested geometry, measured performance, and an architecture that stays maintainable. Correctness, simplicity and measured results take priority over feature count.

### 1.2 Reference user and workflow

- Reference user: the author, validating technical viability. Persona: a drafter sketching a simple floor plan.
- Workflow: create layers → draw line / rectangle / circle / polyline → annotate with text and dimensions → select, move, edit numerically, delete → pan and zoom with grid and snap → save → reopen. Performance data is observable throughout.
- Interaction model: mouse tools plus a numeric property inspector. No command line, no scripting language.

### 1.3 Decisions taken (human, 2026-10-13)

| Topic | Decision |
|---|---|
| Project location | `/home/borba/Documents/borba-cad` |
| Interaction | Mouse tools + numeric inspector |
| Reference user | The author, validating viability (minimal but reliable UX) |

### 1.4 Hypotheses

| ID | Hypothesis | Evidence that answers it |
|---|---|---|
| H1 | Interaction (pan, zoom, pick, draw) stays fluid on realistic drawings. | Frame-time and input-to-redraw distributions per baseline workload (§6.4). |
| H2 | SBCL's stop-the-world GC does not cause visible hitches. | GC event durations correlated with frame-time spikes. |
| H3 | A correct geometry kernel is practical in plain Common Lisp. | Passing tests, including degenerate and reference cases; documented tolerance rationale. |
| H4 | Window, GPU and input through CFFI are practical on Linux/Wayland. | Fresh-checkout build and run log; list of foreign libraries and workarounds. |
| H5 | Real-time instrumentation is cheap. | Measured overhead: instrumentation off vs. on, same workload. |
| H6 | The codebase stays maintainable and extensible. | A documented checklist for adding an entity type, followed when the later entity types are implemented. |

At MVP completion each hypothesis gets a verdict (supported / not supported / inconclusive) backed by measurements in `docs/experiment-report.md`. No verdict without data.

## 2. Platform and dependency constraints

### 2.1 Verified on this machine (2026-10-13)

- Arch Linux x86_64, kernel 7.2.9; KDE Plasma, Wayland session (XWayland on `:0`); NVIDIA GeForce RTX 4060.
- SBCL 2.6.9 (threads), ASDF 3.3.1 (bundled), Quicklisp dist 2026-01-01, GNU Make 4.4.1, gcc, pkg-config, git, gdb. No `perf`, no `valgrind`.
- Native libraries installed: SDL3 and `sdl2-compat` (`libSDL2-2.0.so.0` 2.32.74, an SDL2 API layer over SDL3), GTK4, GTK3, Qt6, Cairo, Pango, FreeType, HarfBuzz, OpenGL/EGL/GLES/Vulkan. GLFW is not installed.
- SBCL defaults here: heap (`dynamic-space-size`) 1 GiB; `bytes-consed-between-gcs` 51 MiB; float traps enabled for overflow, invalid and divide-by-zero.
- The Quicklisp dist provides `cffi`, `float-features`, `bordeaux-threads`, `alexandria`, `fiveam`, `parachute`, `sdl2` (release `cl-sdl2`), `sdl2-ttf`, `cl-opengl`, `cl-glfw3`, `cl-cairo2`, `mcclim`, `cl-freetype2`, `zpb-ttf`. It has no systems named `cl-gtk4`, `sdl3`, `cl-sdl3`, `cl-raylib` or `cl-nuklear`. Presence in the dist says nothing about maintenance or fitness; that is verified in Design.

### 2.2 Constraints

- **Target**: Linux x86_64 and SBCL only. SBCL-specific APIs are allowed but confined to one package. Windows and macOS are out of scope for the MVP; platform code sits behind a small interface so they are not precluded.
- **Language**: all application logic in Common Lisp. Native code only through CFFI, only for window, GPU, input and font services. No separate C++/C#/Rust/Python/Java layer.
- **Dependencies**: minimal. Each candidate passes a verification checklist (maintenance, compatibility with SBCL 2.6.9 and Wayland, license, build and distribution practicality) and is recorded with its rationale in `docs/architecture.md` before first use. Every new dependency is agreed with the human before first use.
- **Layering**: domain, geometry, viewport math, persistence and instrumentation have no GUI or foreign-library dependency (NFR-2). The GUI stack is chosen in Design, after the spikes in §10.5.
- **Artifacts**: fasls, logs, benchmark output and executables stay inside the project (`build/`, `var/log/`, `bench/reports/`) and are git-ignored.

## 3. Coordinate system and units

Full definitions and tolerance rationale go to `docs/geometry.md` (Design deliverable). This specification fixes the rules:

- **World space**: 2D right-handed Cartesian, +X right, +Y up. Angles in radians, counter-clockwise from +X (the UI shows degrees).
- **Unit**: the canonical length unit is the **millimetre**, stored as `double-float`. The file records the unit explicitly. Conversion to display units happens at the UI boundary only, in a single `units` module. *(D3)*
- **Screen space**: logical window pixels, origin top-left, +Y down. World↔screen conversion exists only in the viewport module and carries the display scale (framebuffer pixels = logical pixels × scale). World and screen points are distinct types so they cannot be mixed silently.
- **Range**: coordinates must be finite and within a documented magnitude limit (proposal: ±10 km). Entities violating it are rejected.
- **Tolerances**: separate, centralized, documented constants for length coincidence, angle comparison and relative comparison of derived quantities. Pixel-based constants (pick radius, snap radius) live in the UI layer and never mix with geometric tolerances. Each value has a written rationale and boundary tests; values are chosen in Design, none is arbitrary.

## 4. MVP scope

### 4.1 Entities

| Entity | Definition | Invariants (violations are rejected with a typed error) |
|---|---|---|
| Layer | name, color (RGB), visible, locked | Name unique and non-empty. A default layer always exists and cannot be deleted. A layer holding entities cannot be deleted. |
| Line | start, end | Finite coordinates; length > length tolerance. |
| Rectangle | axis-aligned; min corner, width, height | Width and height > length tolerance. Corners given in any order are normalized. |
| Circle | center, radius | Radius > length tolerance. |
| Polyline | ≥ 2 vertices, `closed` flag, straight segments | Closed requires ≥ 3 vertices. No consecutive coincident vertices. |
| Text | insertion point, string, height | Non-empty single-line UTF-8; height > 0; no rotation. |
| Dimension | aligned linear: two definition points, offset, text height | Points not coincident; text height > 0. The measured value is derived, never stored. |

Every entity has a unique id (never reused within a drawing) and exactly one layer. Color comes from the layer: no per-entity color, linetype or lineweight in the MVP. Creation order defines stacking order.

### 4.2 Functional requirements

- **FR-1 Create.** Line (2 clicks), Rectangle (2 opposite corners), Circle (center, then radius point), Polyline (vertex clicks; Enter finishes; closing gesture defined in Design), Text (click, then type), Dimension (2 points, then an offset click). Live preview while drawing; Esc or right click cancels. Points honor snap.
- **FR-2 Edit.** Move the selection by dragging (snap-aware). Every property is editable numerically in the inspector; invalid input is rejected with a message and the previous value is kept. An entity's layer is changeable.
- **FR-3 Select.** A click picks the nearest entity within a pick radius defined in screen pixels and converted to world units at the current zoom; ties go to the latest created. Shift+click toggles. A click on empty space, or Esc, clears. Ctrl+A selects all pickable entities. Dragging on empty space selects entities fully inside the rectangle (window selection). Entities on hidden or locked layers are not pickable.
- **FR-4 Delete.** The Delete key removes the selection.
- **FR-5 Viewport.** Pan (middle-drag or Space + left-drag), zoom at cursor (wheel), zoom extents. The world point under the cursor stays fixed while zooming. Zoom limits are documented.
- **FR-6 Grid.** Adaptive display (coarsened or hidden when lines would be closer than a documented pixel spacing). Snap-to-grid toggle. Spacing configurable per drawing.
- **FR-7 Inspector.** Shows type, id, layer, editable properties and read-only derived values (Line: length, angle; Rectangle: area; Circle: circumference, area; Polyline: length; Dimension: measured length). Multi-selection shows the count and the common layer.
- **FR-8 Layers.** Create, rename, recolor, show/hide, lock/unlock, set current, delete when empty.
- **FR-9 Files.** New, Open, Save, Save As. Unsaved changes prompt before being discarded. A failed Open leaves the current drawing untouched. Save is atomic (temporary file + rename).
- **FR-10 Errors.** Every failure shows a message (what failed, why, what to do) in the UI and in the log. No silent suppression.
- **FR-11 Startup and configuration.** `make run` launches the app, with an optional file argument. One central configuration (log directory, verbosity, sampling, project root) with environment/CLI overrides.
- **FR-12 Observability.** See §6.

Open decisions that may add requirements: undo/redo (D1) and object snap (D2).

### 4.3 Input operations

- Mouse: left click (tool action / select), left drag (move selection / window select), middle drag or Space + left drag (pan), wheel (zoom), right click or Esc (cancel).
- Keyboard: tool shortcuts (L line, R rectangle, C circle, P polyline, T text, D dimension; Esc returns to select), Enter (finish), Delete, Ctrl+A, Ctrl+N / O / S, Ctrl+Shift+S. The final keymap is fixed in Design.
- Text input: UTF-8, including accented Latin characters, for Text entities and inspector fields.

### 4.4 Persistence

- Native `.bcad` format: UTF-8 text, a single S-expression, explicit integer `format-version`; human-readable and diffable.
- Stored: version, unit, grid settings, layers (and the current layer), entities (id, type, layer, properties), next-id counter. The viewport is not stored: zoom extents on open.
- Loading never evaluates code. Unknown keys, unknown entity types, wrong types, non-finite numbers, duplicate ids, dangling layer references and unsupported versions are errors that name the location. Nothing is silently dropped.
- Evolution: each format change bumps the version and ships an explicit, tested migration. Files from a newer version are refused with a clear message.
- Doubles round-trip exactly (bitwise); tested.
- No DXF, DWG or IFC support is promised.

## 5. Non-functional requirements

- **NFR-1 Correctness first.** Floating-point policy and tolerances are documented and tested (§3). Degenerate geometry is handled explicitly.
- **NFR-2 Layering.** Domain, geometry, viewport math, persistence and instrumentation load and test headless (no window, GPU or foreign library). GUI/platform code is a separate ASDF system that depends on the core, never the reverse; the dependency closure of the core systems is checked automatically.
- **NFR-3 Determinism.** Correctness tests do not depend on screen size, timing or machine. Random data uses a seeded PRNG.
- **NFR-4 Code standards.** English only. Idiomatic Common Lisp: one package per module with explicit exports; CLOS and the condition system where they fit; no placeholders, fake metrics or silent error handling; global mutable state only when justified and documented. Docstring on every exported symbol, one line per parameter in the lab-template format (`PARAM: one-line description`). Full rebuild with zero WARNING and zero STYLE-WARNING.
- **NFR-5 Reproducibility.** A fresh checkout builds and tests with the documented commands. The Makefile never hides failures.
- **NFR-6 Resource bounds.** Log files are rotated and capped. Repeated create/delete cycles do not grow memory without bound (soak benchmark).
- **NFR-7 Load safety.** Loading a malformed file cannot execute code or exhaust memory; limits are documented.

## 6. Performance observability

### 6.1 Destinations

1. Human-readable log file written continuously under `var/log/` (configurable), rotated by size (file count and size set in configuration, with rationale).
2. Live in-app overlay (toggle key): rolling frame stats, last operation durations, entity count, GC, memory. Included only if its own cost is measured and reported.
3. Without a GUI: the log is plain text (`tail -f`, `grep`); `make logs-tail` and `make logs-summary` (count / total / p50 / p95 / max per event) run headless.

### 6.2 Entry format

```
2026-10-13T21:00:00.123Z INFO geometry.hit-test duration_ms=0.412 entity_count=1500 result=success
```

Illustrative. Timestamps are UTC wall-clock at execution time; durations come from a monotonic clock. Errors add `error=<condition-type> message="..."`.

### 6.3 Required measurements

| Metric | Event | Source on SBCL 2.6.9 | Availability |
|---|---|---|---|
| Startup | `app.startup` | Monotonic time from `main` entry to first presented frame, by phase. Excludes core and ASDF load (timed by the Makefile). | Measurable |
| Input event processing | `input.event` | Handler duration, aggregated per event type per interval. Mouse moves are never logged individually. | Measurable |
| Viewport updates | `viewport.update` | Aggregated. | Measurable |
| Frame timing | `render.frame` | CPU frame time and frame interval, aggregated per second (count, mean, p95, max). | CPU: measurable. GPU: only if the chosen renderer exposes reliable timer queries; otherwise reported unavailable. |
| Full / incremental redraw | `render.full-redraw` | As above. | Full: measurable. Incremental: not implemented in the MVP, reported "not applicable". |
| Geometry operations | `geometry.<op>` | Per operation, with workload size. | Measurable |
| Entity create / delete | `entity.create`, `entity.delete` | Per operation, with kind and count. | Measurable |
| Selection / hit testing | `selection.pick`, `selection.window` | With entities scanned. | Measurable |
| File load / save | `file.load`, `file.save` | By phase (read, parse, validate, build); bytes and entity count. | Measurable |
| GC | `gc.event` | `sb-ext:*after-gc-hooks*` plus deltas of `sb-ext:*gc-run-time*` (µs). Hook firing and counter growth are verified; per-event pause accuracy is validated in spike S2 before it is reported as a pause. | Pending S2 |
| Memory | `memory.sample` | `sb-kernel:dynamic-usage` (Lisp heap), `sb-ext:get-bytes-consed` (allocation volume), `/proc/self/statm` (RSS). Sampled periodically, never per frame. | Measurable |
| Benchmark execution | `bench.run` | Per scenario. | Measurable |

Verified clock facts (this machine): `get-internal-real-time` advances in ~1 ms ticks (12 distinct values in 12.6 ms), so it is **not** used for durations. `sb-unix:clock-gettime` with `sb-unix:clock-monotonic` (both exported) showed a smallest non-zero step of 16 ns at ~16 ns per read. Wall-clock timestamps come from `sb-ext:get-time-of-day` (µs). All SBCL-specific calls are wrapped behind one small package.

### 6.4 Rules, workloads and targets

- Instrumentation uses the monotonic clock, supports verbosity levels (error, warn, info, debug, trace) and per-event sampling, and aggregates frame statistics. It never logs every mouse move or frame at default verbosity.
- Estimates are labeled `estimated=true` or not reported. An unmeasurable metric is reported as unavailable, with the reason.
- No optimization is claimed without before/after measurements on the same workload.
- **Baseline workloads** (deterministic generator, fixed seeds, mixed entity types): W0 empty drawing, W1 1,000 entities, W2 10,000, W3 100,000. Scenarios: pan/zoom sweep, random picks, window selection, create/delete burst, save/load, long-running soak.
- **Targets** for interactive responsiveness, geometry, rendering, file operations and memory are proposed to the human only after the baseline report exists. No numbers are promised before that.
- Reports live in `bench/reports/` with an environment fingerprint (SBCL version, CPU, kernel, git revision, heap size). Instrumentation overhead (off / info / debug) is measured A/B and published in `docs/performance.md`.

## 7. Testing and developer workflow

- **Separate systems**: `borba-cad/tests` (correctness, headless, deterministic) and `borba-cad/bench` (benchmarks, never a correctness gate). Checks that need a display report an explicit `SKIPPED (no display)`, never a silent pass.
- **Coverage**: geometry (normal, edge, degenerate, invalid, independent reference cases); entity invariants; coordinate transforms (round trip, zoom-at-cursor invariance, extreme zoom); selection and hit testing; snap; persistence (round trips on seeded random drawings, corrupted-file cases, atomic-save failure); logging (format, rotation, sampling); the rendering boundary through a recording renderer that captures draw commands with no GPU; the input→document pipeline with scripted event sequences; a named regression test per discovered bug.
- **Static gates**: zero warnings on full rebuild; every exported symbol documented, with every parameter named in its docstring; no placeholder markers in shipped code.
- **Framework**: see D4.

### 7.1 Makefile

Wraps ASDF and `sbcl --non-interactive`; fasls are redirected into `build/`. No undocumented REPL steps; every target fails loudly. It is created in the first implementation increment and grows with the project.

| Target | Purpose |
|---|---|
| `make help` | List targets (default goal). |
| `make check-env` | Verify SBCL version, ASDF, Quicklisp dist version, native libraries and display session, with actionable messages. |
| `make deps` | Fetch and pin dependencies (after D5). |
| `make build` | Full rebuild with warnings as failures; produce the standalone executable in `build/`. |
| `make run` | Launch the app (`FILE=` and `LOG_LEVEL=` optional). |
| `make repl` | SBCL REPL with the system loaded. |
| `make debug` | Run with the interactive debugger, debug-level logging and explicit heap size. |
| `make test` | Full correctness suite. |
| `make lint` | Static gates: zero warnings, docstring coverage, no placeholders. |
| `make bench` | Benchmarks; report written to `bench/reports/`. |
| `make logs-tail` / `make logs-summary` | Follow the log / aggregate it per event, headless. |
| `make docs` | Generate the API reference from docstrings (tool chosen in Design). |
| `make clean` / `make distclean` | Remove build artifacts / also logs and reports. |

## 8. Explicit exclusions (MVP)

- From the brief: 3D, BIM, IFC, DWG (and no DXF promise), collaboration or networking, cloud storage, authentication, billing, plugin marketplaces, AI, decorative effects, a custom scripting language, speculative frameworks, any web or browser UI.
- MVP-specific: Windows/macOS; command line; arcs, splines, hatches, blocks/components, linetypes, lineweights, per-entity color; rotated rectangles and text; multi-line text; crossing selection; grip editing; trim/extend/offset/fillet; copy/paste; printing, plotting and export; spatial index and incremental redraw (added only if baseline data proves the need); asynchronous I/O; localization (the UI is English).

## 9. Acceptance criteria

Each criterion is verified by a command or a test.

- **AC-1 Fresh checkout.** Following the README on a clean checkout, `make check-env`, `make test` and `make run` succeed; the window opens on the KDE Wayland session.
- **AC-2 Entities.** All entity types and layers can be created and edited through the GUI; every invariant in §4.1 has tests for normal, edge and invalid input.
- **AC-3 Transforms.** World↔screen round trip stays within the documented tolerance across the documented zoom and pan range; zoom-at-cursor keeps its anchor fixed (numeric assertion). Domain and geometry packages contain no screen or pixel types.
- **AC-4 Geometry.** Each geometry operation has normal, edge, degenerate and invalid-input tests plus at least one independent reference case. Tolerance rationale is documented in `docs/geometry.md`.
- **AC-5 Selection.** The pick radius behaves consistently across zoom levels. Toggle, clear, select-all, window selection and layer visibility/lock rules are tested. Delete removes exactly the selection.
- **AC-6 Snap.** Snapped coordinates are exact grid multiples; with snap off, raw coordinates are returned.
- **AC-7 Persistence.** Save→load reproduces the drawing exactly (including doubles) for seeded random drawings. Each corruption class in §4.4 yields a typed error with location. A failed Open leaves the current drawing intact; a failed Save leaves the previous file intact.
- **AC-8 Logging.** A real session writes a log containing every available metric in §6.3 with timestamp, severity, event, duration and workload. Rotation caps disk use (tested with small limits). A scripted interaction produces no per-mouse-move or per-frame lines at default verbosity.
- **AC-9 Live view and CLI.** The overlay shows frame stats, last-operation durations, entity count, GC and memory. `make logs-summary` aggregates a log without the GUI.
- **AC-10 Benchmarks.** `make bench` runs W0–W3 deterministically and writes a report with the environment fingerprint. Two runs generate identical workloads (matching checksums). Benchmarks are excluded from `make test`.
- **AC-11 Metric matrix.** `docs/performance.md` lists every metric in §6.3 as measured or unavailable, with reason and validation evidence, and reports the instrumentation overhead.
- **AC-12 Build quality.** Full rebuild has zero WARNING and zero STYLE-WARNING; `make build` produces a standalone executable that launches.
- **AC-13 Language and honesty.** All code, docs, logs and UI text are English. No placeholder or fake-metric code ships (checked by `make lint`). Docs describe only implemented features.
- **AC-14 Documentation set.** `README.md`, `CHANGELOG.md`, `docs/architecture.md`, `docs/performance.md`, `docs/geometry.md`, `docs/development.md` and `docs/experiment-report.md` exist and match the code. `LICENSE` only after the human chooses one.
- **AC-15 Sign-off.** The human reviews these criteria and the final experiment report.

## 10. Review (Phase 3)

### 10.1 Missing requirements

| Gap | Why it matters | Proposal |
|---|---|---|
| Undo / redo | Not in the brief. Editing without it is impractical, and it exercises state management, a core hypothesis. | Include basic undo/redo (create, delete, move, property edit). **D1** |
| Object snap | With grid snap only, dimensions of existing off-grid geometry are impossible. | Include endpoint and center snap only. **D2** |
| Display scale | Wayland compositors, KDE included, may use fractional scaling; logical vs. framebuffer pixels is another pixel/world trap. | Specified in §3. |
| Heap size | The SBCL default heap here is 1 GiB; larger workloads or soak runs may hit it. | The Makefile sets `--dynamic-space-size` explicitly; the value is chosen from the baseline. |
| Text input and fonts | Accented characters, caret and fonts are the hidden cost of a custom-drawn UI. | Resolve in spike S3 before choosing the GUI stack. |
| Dependency reproducibility | The Quicklisp dist is dated 2026-01-01; libraries can drift. | Pin and check the dist version. **D5** |

### 10.2 Risks and uncertainties

| # | Risk / uncertainty | Mitigation |
|---|---|---|
| R1 | GUI/rendering stack maturity under SBCL 2.6.9 on Wayland + NVIDIA. `sdl2-compat` is a layer over SDL3; `cl-gtk4` is outside the Quicklisp dist; McCLIM's speed on 10⁴–10⁵ primitives is unknown. | Renderer behind a small protocol (recording renderer for tests). Time-boxed spikes S1 and S3 before Design. |
| R2 | Float traps: SBCL traps overflow/invalid/divide-by-zero by default, and some graphics stacks execute floating-point operations that trap. | Mask traps at the FFI boundary only; keep them enabled in geometry code to catch NaN/infinity early. Verify in S1. |
| R3 | GC pauses vs. frame budget: SBCL's collector is stop-the-world, and 100k boxed entities enlarge the heap. | Measure first (S2, baseline). Evaluate levers afterwards: unboxed coordinate storage, allocation-free hot paths, nursery size. |
| R4 | A standalone executable (`save-lisp-and-die`) must reopen foreign libraries at startup. | Spike S4; document the launch procedure. |
| R5 | Wayland specifics: decorations, cursor, clipboard, fractional scale. | Covered by S1; fall back to XWayland only if justified and documented. |
| R6 | Strategic: professional CAD users are mostly on Windows. | Keep platform code behind an interface; record it as a post-MVP question in the experiment report. |
| R7 | Dimension and Text are the costliest entities (fonts, placement, hit-testing). | Minimal scope (§4.1); implemented last. |

### 10.3 Over-ambitious items (cut first if time-boxes slip)

The live overlay beyond a text view; standalone packaging (half of AC-12); the 100k-entity workload as an interactive promise (it stays a measurement); the Dimension entity.

### 10.4 Decisions needed from the human

Resolution (2026-10-13): the human approved the specification, accepted every recommended default and authorized the spikes.

| ID | Decision | Resolution (recommended default, accepted) |
|---|---|---|
| D1 | Undo/redo in the MVP? | Yes: create, delete, move, property edit. |
| D2 | Object snap in the MVP? | Yes: endpoint and center only. |
| D3 | Canonical unit | Millimetre (typical dimensions such as 150 mm are exact in binary floating point; 0.15 m is not). |
| D4 | Test framework | FiveAM as a test-only dependency (present in the dist), instead of a hand-rolled harness like the lab template. |
| D5 | Dependency acquisition | Quicklisp with the dist version pinned and verified by `make check-env`. Alternative: vendored sources (more reproducible, heavier). |
| D6 | Scope trims | Dimension = aligned linear only; Text single-line, no rotation; Rectangle axis-aligned; window selection only. |
| D7 | File chooser | In-app path prompt vs. native dialog; decided after S3. |
| D8 | Git | `git init` plus `.gitignore` in the first implementation increment; no commits unless asked. |
| D9 | License | Undecided; no `LICENSE` file until chosen. |
| D10 | Spikes S1–S4 | Approve: they download candidate libraries through Quicklisp into `~/quicklisp`; throwaway code lives under `spikes/`. |

### 10.5 Proposed next step: time-boxed spikes (needs D10)

Throwaway experiments that retire the top risks before the Design is written. Findings go into the Design; the code is deleted or archived.

- **S1 Window + GPU + input**: from SBCL through CFFI on Wayland, open a window with a GL context, receive mouse, wheel, key and text events, draw 10,000 line segments, close cleanly; check float traps. Candidates: `sdl2` + `cl-opengl` over `sdl2-compat`; others per the verification checklist.
- **S2 Metrics**: validate clock, GC-event and memory sources under allocation load; confirm the availability column of §6.3.
- **S3 Text**: render UTF-8 text with accents at arbitrary scale and accept typed input.
- **S4 Packaging**: a standalone executable that reopens foreign libraries and shows the window.

Then Phase 4 (Design) for approval: package boundaries, data structures, geometry algorithms, rendering, input model, persistence, logging, testing and build, including the dependency verification table (status, license, compatibility).
