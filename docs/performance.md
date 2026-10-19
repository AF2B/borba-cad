# Borba CAD — Performance

What is measured, how, what the measurements say, and what is not known. Every number below was
produced by this project's own tools on the machine described in §1 and can be reproduced with the
commands in §10. Nothing is estimated; a quantity that cannot be measured is listed as unavailable
with the reason (§2).

Targets (§9) are **proposals for the human to accept or change**; the specification forbids
promising numbers before a baseline exists, and none is asserted by the code or the reports.

## 1. Environment of the recorded baseline

| | |
|---|---|
| Machine | 12th Gen Intel Core i5-12400F, NVIDIA GeForce RTX 4060, Arch Linux, kernel 7.2.9-arch1-1 |
| Display | KDE Plasma, Wayland session (XWayland also present), OpenGL renderer `NVIDIA GeForce RTX 4060/PCIe/SSE2` |
| Lisp | SBCL 2.6.9, heap `--dynamic-space-size 4GB` (set by the Makefile), `bytes-consed-between-gcs` 204 MiB (5 % of the heap) |
| Revision | the repository has no commit yet (the report says so); the code is the working tree of 2026-10-14 |
| Seed | 20261013 (every workload) |
| Reports | `bench/reports/baseline/20261014T042901399Z-borba.txt` (headless), `bench/reports/baseline/20261014T042902817Z-borba-gui.txt` (windowed), `bench/reports/baseline/overhead-5-runs/` (five more overhead runs) |
| Real session | `docs/evidence/demo-session-debug.log`: the scripted demo at debug level in a real window |

## 2. Metric matrix

Every metric required by the specification (§6.3), with its status, source and the evidence that
the source is trustworthy. Events are written to `var/log/borba-cad.log` in the format
`TIMESTAMP SEVERITY event key=value ...`; timestamps are UTC wall-clock, durations come from the
monotonic clock (`clock_gettime(CLOCK_MONOTONIC)`, 16 ns per read).

| Metric | Event(s) | Status | Source and notes | Evidence |
|---|---|---|---|---|
| Startup | `app.startup` | **Measured** | Monotonic time from `main` entry to the first presented frame, by phase (`window_ms`, `context_ms`, `renderer_ms`). Excludes loading the Lisp image and ASDF. | Source run: 253.5 ms (window 134.3, context 86.3, renderer 1.3). Standalone executable: 122.0 ms (window 36.6, context 41.8, renderer 29.1). |
| Input event processing | `input.event` | **Measured** | Handler duration, aggregated per event type per interval (count, mean, p95, max). Mouse moves are never logged one by one (tested). | e.g. `type=pointer-move count=164 handle_ms_mean=0.021 handle_ms_p95=0.041` |
| Viewport updates | `viewport.update` | **Measured** | Aggregated per interval. | `count=14 update_ms_mean=0.003` |
| Frame timing, CPU | `render.frame` | **Measured** | CPU time to apply input, build and submit a frame, swap time, and interval between presented frames; count, mean, p95, max per second. | 75 fps session: `cpu_ms_mean=0.959 interval_ms_mean=13.304` |
| Frame timing, GPU | (benchmark only) | **Unavailable as a vendor timer** | No timer queries are used. The windowed benchmark reports instead the time `glFinish` blocks after the submit ("gpu wait"), which is the GPU work not overlapped with the CPU; it is an upper-bound proxy, not a GPU-timer measurement. | §6 |
| Full redraw | `render.full-redraw` | **Measured** (debug level) | Whole-frame scene and panel build per redraw, with entity, drawn and vertex counts. | 194 entries in the evidence session |
| Incremental redraw | — | **Not applicable** | Not implemented: the draw list is rebuilt whenever the scene is dirty (decision A11). | — |
| Geometry operations | `geometry.pick`, `geometry.window-select`, `entity.copy` | **Measured** (debug level) | Duration with the number of entities scanned. The specification named them `selection.*`; the implementation uses `geometry.*`. | `geometry.pick duration_ms=0.111 entity_count=10` |
| Entity create, delete, edit, paste | `entity.create`, `entity.delete`, `entity.edit`, `entity.paste`, `history.undo`, `history.redo` | **Measured** | Per operation, with the number of changed entities and the entity count. | `entity.delete duration_ms=0.013 changed=1 entity_count=10` |
| File load and save | `file.load`, `file.save` | **Measured** | By phase: load `read_ms` and `parse_ms` (parsing includes validation and building the drawing); save `serialize_ms` (includes the flush to disk) and `rename_ms`; bytes and entity count. | `file.save ... serialize_ms=1.200 rename_ms=0.205`, `file.load ... read_ms=0.155 parse_ms=0.280` |
| Console, scripts and exports | `console.eval`, `script.run`, `file.export-script`, `file.save-recording`, `file.import-dxf`, `file.export-dxf`, `file.export-image` | **Measured**, not in the recorded baseline | One entry each, with the number of expressions run (`forms`), the entities changed (`changed`) and the result (`ok`, `error`, `timeout`) for a command or a script, and bytes and entity count for a file (the picture of the canvas says its width and height; one of 2560 x 1440 pixels is encoded in about 0.4 s, measured once, on the machine of the baseline). The program does not wait for a command longer than its limit (10 seconds, 60 for a script), so the entry is also the proof that it stopped. | none recorded: these events came after the baseline of 1.0.0, and the DXF reader and importer have not been benchmarked |
| GC | `gc.event` | **Measured as GC CPU time** | `sb-ext:*after-gc-hooks*` plus the delta of `sb-ext:*gc-run-time*` (microseconds). The hook runs in SBCL's finalizer thread and only pushes onto a lock-free list; the main loop drains it. Pauses of 5 ms or more are logged at info level, shorter ones at debug. It is CPU time of the collector, not wall-clock stop time; with the single working thread of the program they coincide to the precision of interest. | Spike S2: `*gc-run-time*` tracked an independent stall detector (86.3 ms against 85.9 ms). `*gc-real-time*` has only millisecond resolution and is not used. |
| Memory | `memory.sample` | **Measured** | `sb-kernel:dynamic-usage` (Lisp heap), `sb-ext:get-bytes-consed` (allocation volume), `/proc/self/statm` (resident set). Sampled every 5 s, never per frame. | `heap_mib=103.887 rss_mib=414.539 allocated_total_mib=688.830` |
| Benchmark execution | `bench.run` | **Measured** | One entry per scenario in `var/log/benchmark.log`, with workload and entity count: 9 per workload (8 scenarios and the memory soak), one for the overhead measurement, one per windowed run. | `bench.run scenario="windowed frames, swap interval 1" workload=w3 entity_count=100000 duration_ms=20071.511 result=success` |

The evidence session contains one entry of every logged event above; the file is
`docs/evidence/demo-session-debug.log`. At the default info level a scripted interaction produces
no per-mouse-move and no per-frame lines: the test
`the-demo-scenario-draws-a-complete-plan-without-errors` runs the whole demo with telemetry on and
asserts that its hundreds of pointer moves leave at most one aggregated entry and that no redraw or
pick is logged one by one; `input-and-viewport-summaries-are-aggregated-per-type` and
`frame-summaries-are-written-once-per-interval` check the aggregation itself.

## 3. Workloads

Deterministic drawings built by the seeded generator in `test-support/workload.lisp` (SplitMix64);
the same seed and size give the same drawing on every machine. They are vector drawings: there
are no images. Each is a mix of 45 % lines, 15 % polylines of 3 to 8 open vertices, 15 %
rectangles, 10 % circles, 10 % texts and 5 % aligned dimensions, spread over the default layer and
five named layers, on a square site whose side grows with the square root of the entity count
(constant density).

| Workload | Entities | Checksum of the drawing text (FNV-1a 64) | Generation | Retained heap | Per entity | File size |
|---|---|---|---|---|---|---|
| W0 | 0 | `da2b99ebb82ba67b` | 0.1 ms | 0.2 MiB | n/a | 722 B |
| W1 | 1,000 | `85c048796a1ecabf` | 1.9 ms | 0.4 MiB | 370 B | 96,151 B |
| W2 | 10,000 | `6b33baf5ca5c1406` | 22.9 ms | 2.5 MiB | 263 B | 998,081 B |
| W3 | 100,000 | `fd02a50b099af56a` | 273.9 ms | 24.8 MiB | 260 B | 10,209,345 B |

Equal checksums in two runs prove that the same drawing was measured; they were identical in every
run recorded here (for example W1 in all runs since the first harness test). Retained heap is the
growth of the heap after a full GC when the drawing exists; the file holds about 100 bytes per
entity.

## 4. Baseline, headless

Scene building is the CPU side of a frame without the GPU: it turns a drawing and a viewport into a
draw list (vertices, text runs). The viewport is 1440 × 900. "All entities in view" is the
zoom-extents view, the worst case for drawing. "Pan sweep" visits 60 positions at the working zoom,
where a tenth of the site width is visible. "Zoom sweep" zooms in from the whole site in 40 steps
of one wheel notch (a factor of 1.2 each).
Times are per call; each row is one process; sample counts shrink with the workload to keep the run
bounded (shown in the reports).

| Operation | W1 mean (p95) | W2 mean (p95) | W3 mean (p95) |
|---|---|---|---|
| Scene, all entities in view | 0.430 ms (0.485) | 3.14 ms (3.83) | 36.3 ms (45.2) |
| Scene, pan sweep at working zoom | 29.1 µs (40.7) | 268 µs (311) | 3.27 ms (3.71) |
| Scene, zoom sweep from the whole site | 78.3 µs (320) | 763 µs (3,020) | 18.7 ms (55.6) |
| Pick the entity under a random point | 5.2 µs (7.7) | 60.9 µs (91.7) | 1.01 ms (1.78) |
| Window selection (a tenth of the site side) | 9.1 µs (12.3) | 91.2 µs (135) | 1.15 ms (1.48) |
| Object snap lookup | 8.4 µs (11.1) | 82.5 µs (118) | 1.50 ms (1.81) |
| Create one entity (through the history) | 17.2 µs (25.1) | 193 µs (591) | 2.47 ms (4.71) |
| Undo that creation | 5.5 µs (7.1) | 65.9 µs (74.9) | 0.81 ms (1.34) |
| Redo | 17.8 µs (32.4) | 153 µs (167) | 2.76 ms (3.89) |
| Delete one entity | 7.1 µs (10.3) | 72.1 µs (92.1) | 1.10 ms (1.42) |
| Save to disk (serialize, flush, rename) | 2.49 ms | 17.9 ms | 156 ms |
| Load (read, parse, validate, build) | 7.42 ms | 72.6 ms | 775 ms |

W0 (an empty drawing) costs 9.6 µs for a scene build, 0.1 µs to pick, and 2.1 µs to create an
entity. After every load the checksum of the loaded drawing is compared with the saved one; all
four round trips verified.

What the table shows, and no more:

- Scene building grows linearly with the number of entities drawn, at about 0.3 to 0.4 µs per
  entity (W1 0.43 ms for 1,000, W2 3.1 ms for 10,000, W3 36 ms for 100,000). At the working zoom the
  cost falls by an order of magnitude because most entities are culled, but it is still about 30 ns
  per entity: every entity is visited (there is no spatial index, decision A11).
- Pick, window selection and snap scan every entity with a bounds pre-filter and grow linearly:
  5 to 15 ns per entity.
- Create, redo and delete grow roughly tenfold per decade of size, so they are linear in the drawing
  size too (2.5 ms for one creation at 100,000 entities). The cause was not profiled.
- Loading dominates the file operations: 775 ms for 10 MB at W3 (13 MB/s), against 156 ms for saving
  (65 MB/s).
- The zoom sweep has a long tail (p95 far above the mean) because it starts with the whole site in
  view, the expensive case, and ends zoomed in.

### Memory

Repeated create-and-delete cycles through the history (400 cycles of 50 entities each) were
measured with the heap after a full GC at checkpoints:

| Workload | Start | After 100 / 200 / 300 / 400 cycles | History cleared |
|---|---|---|---|
| W0 | 19.1 MiB | 20.2 / 21.4 / 22.0 / 22.0 | 19.1 MiB |
| W1 | 19.4 MiB | 20.5 / 21.7 / 22.3 / 22.3 | 19.4 MiB |
| W2 | 20.9 MiB | 22.1 / 23.3 / 23.9 / 23.9 | 21.0 MiB |
| W3 | 37.0 MiB | 38.2 / 39.4 / 40.0 / 40.0 | 37.0 MiB |

The heap grows while the undo history fills (500 change sets) and is flat afterwards (0.0 MiB
between cycles 300 and 400); clearing the history returns it to the starting value. No leak was
observed (NFR-6). A drawing of 100,000 entities keeps about 25 MiB alive; the whole process needs
well under the 4 GiB heap, whose size was not tuned: the effect of other sizes was not measured.

## 5. Variance between runs

Two runs of the same code are not identical, and the difference is larger than the sample
statistics of one run suggest. Eight separate processes running the final code gave these means
for "scene, all entities in view" at W2: 2.29, 2.49, 2.79, 2.91, 2.96, 3.13, 3.14 (the recorded
baseline) and 3.27 ms, and six
processes running the code before the reformatting of 2026-10-14 gave 2.22 to 2.50 ms. The
scene, viewport, geometry, domain and user-interface sources are token-for-token identical between
the two versions apart from the table that defines the theme colors (checked by comparing their
token streams), so the spread is a property of the process and the machine (memory placement,
frequency), about ±20 %. Consequences: only compare runs when they differ
by clearly more than this; a before/after claim needs several processes, not one; the percentile
columns describe one process only.

## 6. Baseline, windowed

The benchmark opens a real window (1440 × 900 logical pixels, 4× MSAA when offered), feeds the same
input as the headless frame benchmark (a zoom oscillating about the canvas center and a moving
pointer, starting from zoom extents) and renders 600 frames after a warm-up, with the swap interval
at 1 (requested) and 0. Columns: mean, p50, p95, max, in milliseconds.

| Workload (interval) | CPU: input, build, submit | GPU wait (`glFinish`) | Interval between frames | Serialized frame cost | Rate |
|---|---|---|---|---|---|
| W1 (1) | 0.884, 0.801, 1.150, 40.06 | 0.441, 0.160, 1.766, 2.373 | 1.419, 1.177, 2.711, 40.58 | 1.33 | 705 fps |
| W1 (0) | 0.788, 0.777, 1.097, 1.528 | 0.473, 0.153, 1.711, 2.369 | 1.358, 1.125, 2.767, 4.849 | 1.26 | 737 fps |
| W2 (1) | 4.035, 3.836, 6.561, 55.78 | 0.430, 0.231, 1.229, 1.932 | 4.641, 4.419, 7.269, 56.11 | 4.47 | 216 fps |
| W2 (0) | 3.727, 3.740, 5.269, 8.912 | 0.446, 0.268, 1.224, 1.807 | 4.359, 4.342, 6.130, 9.906 | 4.17 | 229 fps |
| W3 (1) | 29.91, 30.55, 39.90, 49.58 | 0.682, 0.663, 1.480, 1.939 | 30.79, 31.20, 41.01, 50.79 | 30.59 | 32.5 fps |
| W3 (0) | 30.49, 31.68, 39.98, 44.84 | 0.678, 0.637, 1.441, 2.069 | 31.37, 32.68, 41.03, 46.08 | 31.17 | 31.9 fps |

How to read it:

- With the swap interval at 1 the swap took 0.1 to 0.2 ms: the swap did not wait for the display in
  this environment, so the "interval" is the rate at which the program submits frames, not a rate
  the display shows. The real sessions (§7) ran at 75 fps, the refresh rate of the display.
- The GPU is not the limit: waiting for it after the submit costs under 1 ms at every size, while
  the CPU work grows with the entities (W3: 30 ms). The frame is CPU-bound in the scene build, as
  §4 predicts.
- Draining the pipeline every frame serializes CPU and GPU, so "serialized frame cost" is an upper
  bound of a real frame's cost.
- With the interval at 1 the CPU maxima of W1 and W2 are 40 and 56 ms while their p95 are 1.2 and
  6.6 ms: isolated frames. With the interval at 0 the maxima are 1.5 and 8.9 ms. The outliers were
  not attributed.
- The input starts from zoom extents and oscillates around it, so the windowed numbers are the
  worst case (almost every entity in view); the working-zoom case was measured headlessly only.

## 7. Real sessions

The scripted demo (a floor plan of 10 entities drawn with every tool, a move, delete and undo, a
save and an open, zoom and pan) ran in a real window on the machine of §1:

- Frame rate 69 to 75 fps, consistent with a 75 Hz display refresh (the refresh rate was not
  queried), CPU per frame 0.96 to 1.32 ms mean, p95 2.4 to 7.5 ms; the frame interval mean was 13.3
  ms.
- Startup to first frame: 253.5 ms from source, 122.0 ms from the standalone executable.
- One garbage collection of 44.2 ms at a 41.7 MiB heap (the only collection of the session; the
  nursery is 204 MiB). The same 4 s summary window contains the longest frame interval
  of the session before the shutdown, 50.5 ms; the log cannot place the pause inside that window
  more precisely, so the match is consistent with, not proof of, a one-frame hitch caused by the
  collection. A frame of 156 ms at the end is the screenshot: the demo encodes a PNG inside the
  last frame.
- In an earlier run the overlay showed two collections with 17.6 ms in total and 15.9 ms at most.

Hitches of one to three frames were therefore observed, and their frequency in long sessions with
large drawings is not known (§9).

## 8. Instrumentation overhead

The same scripted frames (input, scene, panels, and the per-frame telemetry calls of the main loop,
without GPU work) were rendered with no telemetry, with a second identical uninstrumented setup as a
control, with telemetry at the error, info and debug levels, and with info-level telemetry plus
the live overlay drawn on every frame. The six setups run interleaved in ten rounds of 150
frames, the order rotating each round, so that drift hits all equally: 1,500 frames per setup. The
drawing has 1,000 entities; one uninstrumented frame takes about 280 µs.

Difference from the uninstrumented setup, mean µs per frame, in the six independent processes of
the recorded runs (the baseline report and the five runs in `overhead-5-runs/`):

| Setup | Run 1 | 2 | 3 | 4 | 5 | Baseline | Range |
|---|---|---|---|---|---|---|---|
| control (identical, no telemetry) | +13.8 | +3.6 | −3.1 | −4.5 | −2.3 | +5.6 | −4.5 to +13.8 |
| telemetry, error level | +2.8 | +9.5 | −7.4 | +15.3 | +2.3 | +4.7 | −7.4 to +15.3 |
| telemetry, info level | +17.3 | +32.8 | +7.5 | +35.0 | +16.1 | +17.9 | +7.5 to +35.0 |
| telemetry, debug level (1,514 lines, 203 KB per run) | +23.7 | +30.2 | +15.5 | +28.8 | +14.5 | +37.2 | +14.5 to +37.2 |
| info level plus overlay | +15.5 | +15.5 | −5.4 | +7.2 | +1.2 | −1.4 | −5.4 to +15.5 |

Reading: the noise floor, the difference between two identical setups, reaches about ±14 µs. The
levels that write (info and debug) cost about 8 to 37 µs per frame, 3 to 13 % of this 280 µs
CPU-only frame and well under 0.3 % of a 60 Hz frame (16.7 ms). Writing 1,500 debug lines (203 KB)
in 1,500 frames is the largest cost seen. The overlay itself is not distinguishable from zero: the
overlay setup was on average faster than info-level logging alone in every run, which is noise, not
a negative cost. H5 (instrumentation is cheap) is therefore supported, with the stated noise.

## 9. Proposed targets (for the human to decide)

Derived from the baseline of §4 and §6; none has been promised or enforced. The "baseline" column is
what the proposal is compared with. Interactive targets are for the windowed benchmark on this
machine; the file and memory targets for the headless one.

| # | Proposed target | Baseline | Outcome today |
|---|---|---|---|
| T1 | W2 (10,000 entities), everything in view: CPU frame p95 ≤ 8 ms | 6.6 ms (swap 1), 5.3 ms (swap 0) | met |
| T2 | W3 (100,000), everything in view: CPU frame p95 ≤ 50 ms (20 fps) | 39.9 ms | met |
| T3 | W3, everything in view: CPU frame p95 ≤ 16 ms (60 fps) | 39.9 ms | **not met**; needs the optimization planned in decision A11 (retained buffers or a spatial index), measured before and after |
| T4 | W3 at the working zoom: scene build p95 ≤ 8 ms | 3.7 ms (headless) | met for the build; the windowed frame at this zoom is not yet measured |
| T5 | W3: pick, window selection and snap p95 ≤ 2 ms | 1.78, 1.48, 1.81 ms | met, with little margin |
| T6 | W3: one edit (create, delete, undo, redo) p95 ≤ 5 ms | 4.71, 1.42, 1.34, 3.89 ms | met, with little margin |
| T7 | W3: save ≤ 0.25 s, load ≤ 1.0 s | 0.156 s, 0.775 s | met |
| T8 | Retained heap ≤ 300 B per entity; no growth after the history is full or cleared | 260 B; 0.0 MiB | met |
| T9 | GC pause ≤ 16.7 ms in a ten-minute session at W2 | one 44.2 ms pause observed in a ten-second session of 10 entities | **unknown to not met**; needs a long session log (see below) |
| T10 | Telemetry at info level ≤ 50 µs per frame | +8 to +35 µs | met |

Open questions the data cannot answer yet:

1. How often does SBCL pause for tens of milliseconds in a long interactive session with a large
   drawing (H2)? A ten-minute windowed run at W2 and W3 with GC logging, correlated with frame
   intervals, would answer it.
2. Where does the time go inside the scene build, and inside a single edit at W3? Neither was
   profiled; the optimizations named in T3 should start with a profile.
3. Is the 4 GiB heap the right size, or would another nursery size trade pause length against
   frequency?

## 10. Reproducing

```bash
make bench                       # headless W0-W3 + overhead -> bench/reports/<time>-<host>.txt
make bench-gui                   # windowed frame benchmark   -> bench/reports/<time>-<host>-gui.txt
make bench-baseline              # both, into bench/reports/baseline/
make bench-quick                 # tiny run that proves the harness works (numbers not comparable)
make logs-summary LOG_DIR=...    # count, total, mean, p50, p95, max per event of any log directory
```

Run the benchmarks on an idle machine: they measure wall-clock time. Every report starts with the
environment fingerprint (date, host, Lisp version, CPU, kernel, heap size, bytes consed between
GCs, git revision, seed) and the workload checksums. Benchmarks are never part of `make test`; the
tests only check that the harness works (a tiny workload: the report is complete, the round trip is
verified, an unknown workload is refused).

## 11. Not measured

- GPU execution time with a vendor timer, and overdraw.
- Wall-clock GC pauses (the collector's CPU time is measured, §2).
- A windowed frame at the working zoom with W3, and any session longer than a few seconds.
- Energy and power use.
- Other machines, other GPUs, X11 (the session was Wayland).
