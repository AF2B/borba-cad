# Borba CAD — Experiment report

The question: can Common Lisp (SBCL) be the foundation of a professional civil-engineering
software ecosystem? The MVP, a native 2D drafting application, is the instrument; this report
gives a verdict on each hypothesis of the specification (§1.4) with the data behind it, says what
was learned, and lists what the data cannot yet answer. No verdict was given without data.

## 1. Verdicts

| ID | Hypothesis | Verdict | Basis |
|---|---|---|---|
| H1 | Interaction stays fluid on realistic drawings | **Supported, with a limit** | Frame CPU 0.9 ms (1,000 entities) and 4.0 ms (10,000); 30 ms (32 fps) with 100,000 entities all in view. |
| H2 | SBCL's stop-the-world GC causes no visible hitches | **Inconclusive** | One 44 ms pause seen in a 10-second session; no long-session data. |
| H3 | A correct geometry kernel is practical in plain Common Lisp | **Supported** | 110,000+ checks including independent references and randomized property tests; every kernel, domain and persistence bug found was found by a test. |
| H4 | Window, GPU and input through CFFI are practical on Linux/Wayland | **Supported** | Builds and runs from a fresh copy on KDE Wayland with an NVIDIA GPU; four workarounds, all small. |
| H5 | Real-time instrumentation is cheap | **Supported** | At most 37 µs per frame in every setup and run, about 0.2 % of a 60 Hz frame. |
| H6 | The codebase stays maintainable and extensible | **Inconclusive** | A checklist exists and its steps fail loudly, but it was written after the fact and never used for a new type. |

## 2. Evidence

### H1: interaction stays fluid

Windowed benchmark, 1440 × 900, the real renderer, a zoom oscillating about the zoom-extents view
(the worst case: nearly every entity is in view). From
`bench/reports/baseline/20261014T042902817Z-borba-gui.txt`:

| Entities | CPU per frame (mean / p95) | GPU wait | Rate |
|---|---|---|---|
| 1,000 | 0.88 / 1.15 ms | 0.44 ms | 705 fps |
| 10,000 | 4.04 / 6.56 ms | 0.43 ms | 216 fps |
| 100,000 | 29.9 / 39.9 ms | 0.68 ms | 32 fps |

Interaction is fluid up to 10,000 entities even when all of them are drawn. At 100,000 entities it
is fluid only when zoomed in: the scene build at the working zoom (a tenth of the site in view) is
3.3 ms headlessly, against 36 ms with everything in view. The frame is CPU-bound in the scene build
(about 0.3 to 0.4 µs per drawn entity); the GPU is not the limit. The optimization planned in the
architecture (retained buffers or a spatial index) was deliberately not done without a profile.
Picking, window selection and snapping take 1.0 to 1.5 ms at 100,000 entities, editing 0.8 to
2.8 ms, loading a 10 MB file 0.78 s. Details and variance in [performance.md](performance.md).

### H2: GC pauses

The collector's CPU time is measured per collection (the hook and `*gc-run-time*`, validated
against an independent stall detector in spike S2: 86.3 ms against 85.9 ms). In the benchmarks a
whole sweep over a 100,000-entity drawing triggered at most one collection of 1.5 to 2.2 ms. In
the one real session logged at debug level (a small drawing, 8 seconds) the only collection took
44.2 ms, and the frame interval of 50.5 ms in the same window is consistent with a dropped frame.
Spike S2 had also found pauses of 48 to 86 ms with about 110 MiB of live data. The data say that
hitches of one to three frames exist; they cannot say how often they happen in a working session
with a large drawing. A ten-minute windowed run at 10,000 and 100,000 entities with GC logging is
the missing experiment.

### H3: a correct geometry kernel in plain Common Lisp

The kernel (`geometry`) and the domain are plain doubles and CLOS. Correctness rests on 110,256
headless checks (`make test`, about 3 s) that include analytic and hand-computed cases, brute-force
references (distances against sampling, clipping against the definition, picking and window
selection against a scan), seeded randomized property tests (round trips over the whole zoom range,
grid multiples, layout consistency), degenerate and invalid inputs, truncation and mutation fuzzing
of files, a model-based test of the store and the history, and a randomized "monkey" test of input
events. Tolerances are derived from the floating-point representation and checked numerically
([geometry.md](geometry.md)). The bugs the tests found during development were: a text entity that
shared its caller's string, a length bound that rejected valid diagonals, mixed single and double
precision in the snap marker, a double where a single was expected in a layout helper, a label
truncated by one ulp, and a wrong error for non-numeric input. All were found by tests. Four
defects outside the kernel were found only by running the program: toggle labels that did not fit
their button, memory figures that read zero at startup, a truncated hint, and the stalled demo
(under H4). Common Lisp's numeric tower made exact decimal parsing and exact `double-float` round trips easy; the
float traps made a NaN fail where it started.

### H4: window, GPU and input through CFFI on Linux/Wayland

`sdl2` over `sdl2-compat`, `cl-opengl` (OpenGL 3.3 core) and `cl-freetype2` run a real window on
KDE Wayland with an NVIDIA RTX 4060. A copy of the repository without `build/` and `var/` passes
`make check` (strict build of every system including the third-party libraries, lint, 110,256
headless checks, 131 window checks that render known scenes and read the pixels back) in 46 s, and
builds a 67 MB standalone executable that runs. The workarounds found, with their cost:

1. `SDL_PushEvent` of a text-input event crashes `sdl2-compat`: text translation is tested on
   locally built event structures instead of pushed events.
2. `sdl2:with-init` runs its body in another thread, where the caller's handlers do not apply:
   errors are caught there and re-signaled in the caller.
3. `cl-freetype2` creates its library handle while loading, and the handle is a dangling pointer in
   a saved image: the executable opens its own and the build clears the old one.
4. `cl-freetype2` needs `gcc` and the FreeType headers on first load.

Also observed: on this setup a buffer swap with the interval set to 1 did not block (the swap took
0.1 to 0.2 ms in the benchmark), so frame pacing cannot be assumed from the swap; in a visible
window the sessions ran at 75 fps. A window that receives no event (hidden or covered) exposed a
real defect in the demo loop, found by the fresh-copy run and fixed with a regression test: the
scripted demo advanced only on frames and frames came only from events.

### H5: instrumentation is cheap

Six setups, 1,500 frames each, interleaved, repeated in six processes
([performance.md](performance.md) §8). Against no telemetry at all, a 280 µs CPU-only frame cost at
most 37 µs more (info level: 8 to 35 µs, debug level with 1,514 lines written: 15 to 37 µs, overlay
drawn: not distinguishable from zero), with a noise floor of about ±14 µs between two identical
setups. That is under 0.3 % of a 60 Hz frame. Not measured: the cost on a larger drawing, and the
cost of disk I/O under load.

### H6: maintainable and extensible

What exists: entity behavior is a set of generic functions plus a table-driven file format, the
scene dispatches with `etypecase` so a forgotten clause fails at run time, and a checklist for a new
entity type is in [development.md](development.md) §7. The dimension, the last type added, appears
in 14 source files: 3 hold its own code, 4 need a block of a few lines and 7 need a line or a mention.
What is missing for a verdict: the checklist was written from the finished code, not before it,
and no type has been added by following it. The test that would settle it is to add a seventh type
(an arc, say) by the checklist and count the surprises. Other maintainability facts: lint enforces
documentation of every exported symbol, a layout rule and the core's dependency rule; the strict
build has zero warnings; the largest source files are `src/app/panels.lisp` (856 lines), `src/platform/main.lisp` (735) and
`src/scene/scene.lisp` (658).

## 3. What was learned

- A headless core with a draw list as the only boundary paid for itself: 99.9 % of the checks run
  without a window, and the renderer could still be tested with real pixels.
- Measuring before designing changed decisions: the clock, the GC hook and the metric sources were
  validated in spikes before any code relied on them; the frame benchmark showed the swap does not
  pace the program; the benchmark variance (about ±20 % between processes) showed that a single run
  proves little.
- Honest numbers need a control: the identical-setup row in the overhead measurement is what turned
  "the overlay costs −13 µs" into "indistinguishable from noise".
- The expensive part of a CAD frame here is not the GPU but visiting every entity on the CPU; a
  spatial index or retained buffers is the next lever, and a profile should choose between them.
- The style rule "one parameter per line" was enforced by lint on production code (645 calls
  reformatted) and caught nothing but formatting; it also pushed tabular data (colors, the demo
  script) into data tables, which read better than long calls.

## 4. Strategic question (R6)

Professional CAD users are mostly on Windows and macOS. Nothing in this MVP is Linux-specific
except the SBCL support package, the `/proc` memory source, the font search paths and the Makefile;
SDL2, OpenGL 3.3 and FreeType are portable and the platform code sits behind the draw list. Whether
the SBCL toolchain, the foreign libraries and the executable packaging are comfortable on
Windows and macOS was not tried. It is the first question for the next experiment.

## 5. Acceptance criteria

| AC | Criterion | Status | Evidence |
|---|---|---|---|
| 1 | Fresh checkout | Verified on a copy of the repository | `make check` passes in a copy without `build/` and `var/`; `make demo` and the executable run there |
| 2 | Entities through the GUI; invariants tested | Verified | Demo covers every tool; `domain-entities`, `app` suites; screenshot in `var/screenshots/` after `make demo` |
| 3 | Transforms | Verified | `viewport` suite (round trip within 1e-8 mm, zoom at cursor, extreme zoom); `geometry` and `domain` contain no pixel type |
| 4 | Geometry | Verified | `geometry` suite; [geometry.md](geometry.md) |
| 5 | Selection | Verified | `the-pick-radius-is-the-same-number-of-pixels-at-every-zoom`, toggle, clear, select-all, window, layer rules in `app` and `domain-drawing` |
| 6 | Snap | Verified | `grid-snap-produces-exact-multiples-of-the-spacing`, `with-snapping-off-clicks-are-exact`, object snap tests |
| 7 | Persistence | Verified | `persistence` suite: round trips, every corruption class with location, failed open and failed save leave the data intact |
| 8 | Logging | Verified | `docs/evidence/demo-session-debug.log`; rotation tested with small limits; no per-move or per-frame lines at the default level (tested) |
| 9 | Live view and CLI | Verified | Overlay (`Perf` button, `F3`); `make logs-summary` runs without the GUI |
| 10 | Benchmarks | Verified | `make bench` writes the report with the fingerprint; identical checksums in every run; excluded from `make test` |
| 11 | Metric matrix | Verified | [performance.md](performance.md) §2 and §8 |
| 12 | Build quality | Verified | Strict build with zero warnings of seven systems; `make build` produces and the run of `build/borba-cad` shows the window |
| 13 | Language and honesty | Verified by lint and review | Lint finds no placeholder marker or unsafe form; the documents describe implemented features and state what was not measured |
| 14 | Documentation set | Verified | README, CHANGELOG, architecture, performance, geometry, development, this report; no `LICENSE` until the human chooses one |
| 15 | Sign-off | **Pending the human** | Review of these criteria and of this report |

## 6. Limits of this experiment

One machine, one GPU, one display server, one SBCL version; benchmark runs differ by about ±20 %
between processes; no session longer than a few seconds was logged; the entities and tools are the
MVP's (no arcs, no rotated text, window selection only); no user study was done. The conclusions
above hold for those conditions.
