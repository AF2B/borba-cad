# Changelog

Format: [Keep a Changelog](https://keepachangelog.com/). The project follows semantic versioning.

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
