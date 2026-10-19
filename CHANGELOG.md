# Changelog

Format: [Keep a Changelog](https://keepachangelog.com/). The project follows semantic versioning.

## [Unreleased]

### Added

- **Recording what you do as a script.** `F9`, a **Rec** chip in the status bar and More > Record actions start a
  recording, and the same again stops it and asks where to save the script. The script is read from the steps of
  undo that came after the start, so an action that was undone is not in it: new entities are the commands that
  draw them, erased ones `erase`, moved ones `move` (entities moved by the same distance share one command),
  changed properties `edit`, and any other change `redraw`. What the recording made is named by variables
  (`*line-12*`) and what was already there by its number, so the script is run on the drawing as it was when
  the recording began; layers and the blocks that placements use are declared in the script. A recording that
  cannot be told (the drawing was replaced, or what came before it was undone) says so. The console gets
  `redraw`, which puts the entity that a drawing command makes in the place of another, keeping its number, its
  layer and its place in the drawing order.
- **Spread.** `Shift+P` in the Modify menu takes the selected shapes apart and moves each piece 1 cm from the
  others; with nothing selected it waits for one click inside a shape (a number typed before the click sets the
  distance), shows the result under the pointer, and then gives way to Select. A rectangle, polygon or polyline
  becomes its sides, each moved straight out of the shape; a circle becomes four arcs; a block becomes the
  entities it stands for. It is one step of undo, selects the pieces, and a shape with no pieces says so. The
  console gets `spread`.
- **Light theme.** A sun and moon button in the title bar switches between the dark and the light theme, and the
  choice is kept for the next run; `--theme dark|light` and `BORBA_CAD_THEME` override it. On the white canvas
  the colors of the layers are darkened where they would be too pale to see (a near-white layer is near-black),
  in the canvas and in the layer swatches; the file keeps the colors the layers were given.
- **Zoom buttons.** + and - buttons in the top right corner of the canvas zoom by one notch of the wheel about
  the center of the canvas.
- **Blocks.** Make block (`Shift+B`) turns the selection into a named block and one placement of it, Insert block
  (`I`) places a block with a click, a typed scale and a second click for the turn, and Explode (`Shift+X`) takes a
  placement apart. A placement has a position, scale, rotation and flip, takes the layer it is on, is drawn, picked
  and snapped like what it stands for, and goes through rotate, mirror, scale, copy and paste, the script export
  (`with-block`, `insert`), the DXF export (as plain entities) and the PDF and SVG plots. The console gets
  `with-block`, `make-block`, `insert`, `explode` and `blocks`.
- **Hatch and solid fill.** Two tools in the Draw menu fill the smallest closed shape (rectangle, circle, ellipse,
  polygon or closed polyline) under the pointer, with a live preview: Hatch with parallel lines at 45 degrees,
  Solid fill with a solid color. The inspector changes a hatch to lines, cross or solid, and sets its angle and
  spacing; the pattern moves and turns with the shape, and is carried through rotate, mirror, scale, the script
  export (`hatch-polygon`), the console (`hatch`), the DXF export (as lines or filled triangles) and the PDF and
  SVG plots. A spacing that would make more than 5,000 lines is refused.
- **PDF and SVG export.** More > Export as PDF and Export as SVG put the drawing on an A4, A3, A2 or A1 sheet at
  a scale you type (`1:50`, `2:1`, or `fit`), in black or in the colors of the layers, with the sheet turned to
  suit the drawing, a margin, 0.25 mm lines and curves within 0.02 mm of the true ones. The PDF is written by
  hand, without any library; the notice says the sheet and the scale and warns when the drawing does not fit
  or when characters had to become question marks. The last choices are remembered.
- **Trim, extend, offset and fillet.** Four tools in the Modify menu that work on what is clicked. Trim cuts away
  the part of a line, an arc or a circle between the crossings with the other visible entities; extend lengthens
  a line or an arc up to the next entity; offset copies a line, circle, arc, rectangle, polygon or polyline at
  a distance given by a click or typed, on the same layer, with a closed shape growing or shrinking by the side
  that was clicked and corners kept sharp; fillet rounds the corner of two lines with an arc of a remembered
  radius, or only takes them to the corner when the radius is zero. Each is one step of undo, and each says in
  a sentence why it could not be done.
- **Radius, diameter and angle dimensions.** Three new tools in the Draw menu. A radius or diameter dimension is
  made by clicking a circle or an arc and then where the leader should point; its text starts with R or the
  diameter sign and always reads left to right. An angle dimension takes the vertex, a point on each arm and a
  click for the arc, whose side chooses which of the two angles is measured. Each has its grips, its inspector
  rows, the console commands `radius-dimension`, `diameter-dimension` and `angle-dimension`, and is carried
  through rotate, mirror, scale, copy, the script export and the DXF export (as lines, arrowheads and text).
- **Measuring.** A ruler button in the palette opens Distance (`M`), Angle (`Shift+A`) and Area (`Shift+Q`).
  Distance gives the length, the parts along each axis and the direction; angle gives the smaller angle at a
  vertex and the one that completes the turn; area closes a contour of clicked corners (Enter, or the first
  corner) and gives the area and the perimeter. The measured shape and its value are drawn on the canvas, in
  the display unit, until the next measurement or the end of the tool, and nothing is added to the drawing or
  to the history.
- **Rotate, mirror and scale.** A **Modify** button in the palette (and `Shift+R`, `Shift+M`, `Shift+S`) turns,
  reflects or resizes the selection with a live preview. Rotate takes a center and a direction or a typed
  angle, mirror takes two points and asks whether to keep the originals, scale takes a base point and a
  reference and a target or a typed factor. `Ctrl` on the last click keeps the originals and selects the
  copies; each operation is one step of undo; a rectangle turned to a slant becomes a closed polyline;
  entities on locked layers are left alone and the message says so. The console gets `rotate`, `mirror` and
  `scale`, each with `:copy t`.
- **More object snaps.** The OSnap chip now also gives midpoints (of segments, sides and arcs), quadrants (of
  circles and arcs), intersections (of lines, rectangles, polylines, polygons, circles and arcs with each other)
  and, from the last point of the tool, perpendiculars and points of tangency to circles and arcs. Each has
  its own marker and name in the status bar; at the same distance the endpoint wins. The crossings are looked
  for among the 40 entities nearest the pointer, so a crowded drawing stays fast.
- **Ortho, polar and typed points.** `F8` and `F10`, and chips in the status bar, hold the next point of a line,
  polyline, dimension, rectangle, circle, arc, ellipse or polygon to a horizontal or vertical line from the
  last point (ortho) or to multiples of 15 degrees (polar); an object snap still wins. A tool that waits for a
  point also takes it typed: a length toward the pointer, `x;y`, `@x;y` or `length<angle`, in the display unit
  or with `mm`, `cm` or `m`. A text that is not a point is said and kept for correcting.
- **Autosave and recovery.** A drawing with unsaved changes is copied to the state directory every minute
  (only when it changed since the last copy), and the copy is removed when the drawing is saved or closed in
  the usual ways. After a crash the next start asks whether to recover the work: Recover, Discard or Not now.
  A copy that cannot be written is logged and never interrupts the user.

### Changed

- **Explode works on shapes.** Explode (`Shift+X`) used to take only blocks apart and answered "Select a block to
  explode" to anything else; it now also takes a rectangle, polygon or polyline apart into lines, where they were.
- **A version is a tag.** Every tag that reads `vMAJOR.MINOR.PATCH` is a version, and `make version` and the names
  of the distributions come from the nearest version tag (`1.3.0`, or `1.3.0-4-g1a2b3c4` four commits past it, and
  `-dev` where there is no version tag) instead of from `borba-cad.asd`.
- **The pipeline runs on GitHub.** The workflow that only built the distributions is replaced by `ci.yml`:
  version, lint, tests, builds for Linux and Windows, a documentation site deploy and, for a version tag, the
  release with the Linux and Windows programs, their checksums and the notes of the version from this changelog.
  A tag that is not a version, or a version with no notes here, fails in seconds. Runs of `main`, of pull
  requests and by hand keep the programs only as artifacts and no longer make `build-N` tags and pre-releases.
- The demonstration frames the plan at 0.09 px/mm, so that its corner is not under the zoom buttons, and
  `examples/floor-plan.bcad` is regenerated, which brings it to the current format.
- The `.bcad` format is now version 5, which adds the radius, diameter and angle dimensions, the hatch and the
  blocks. Versions 1 to 4 still open and are saved as version 5; a version 4 file cannot be opened by an earlier release
  of the program.

## [1.2.0] - 2026-10-15

### Fixed

- **DXF files with blank lines after `EOF` were refused** ("expected a group code, a whole number, found
  nothing"), which is how many files from the internet end. The reader now stops at the group `0 EOF`,
  skips a blank line where a group code belongs, reads the names of records and sections with spaces
  after them, and ends a file that has no `EOF` at its last line. Found with two files downloaded by the
  user (`minimal_r12.dxf`, `block_example.dxf`), which now import.
- A closed polyline that repeats its first vertex at the end (how many programs write the rectangle of a
  closed polyline) was left out of the import as a segment of no length; it is now read as the closed
  polyline it is.

### Changed

- **Running a script asks where it draws, in plain words.** The question no longer explains what a script
  is: it says "It can draw on a new layer of this drawing, or in a new drawing. Run only files you
  trust." with the buttons **New layer**, **New drawing** and Cancel. *New layer* makes a layer named for
  the file (with a number after it when that name is taken) and the script draws on it, unless it chooses
  its own layers; the layer is taken away again when nothing is left on it, so a script that fails
  leaves nothing behind. *New drawing* offers to save unsaved work first, and runs the script in an
  empty drawing, leaving the one that was open as it was. Both are one step of undo.
- **Exports end with a message in the middle of the window.** Exporting a DXF file or a script shows a
  window that says the file was saved (with its size, and for DXF what changed shape) or why it could not
  be, instead of a line in the status bar that is easy to miss; a script that stops halfway says so in
  the same way.

### Added

- **The console is native Common Lisp, and exact.** What is typed runs in SBCL itself, so `format` (what
  `format t` writes appears in the console), `let`, `defun`, `loop`, `mapcar` and the rest of the language
  work, and a test says so. In the console `sqrt`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `exp`, `log` and
  `float` answer in double-floats even for a whole number, where Common Lisp answers in single-floats with
  seven digits (`(sqrt 2)` was 1.4142135, now 1.4142135623730951): on a drawing 10 km across, seven digits
  are a millimetre. The originals remain as `cl:sqrt` and the rest.
- **Russian (ru-RU).** The language menu lists Русский (Россия) beside English (US) and Português (Brasil);
  `--language ru-RU` and `BORBA_CAD_LANGUAGE=ru-RU` choose it. The catalog has every text of the program, in
  the terms Russian CAD programs use (чертёж, слой, отрезок, привязка, размер); the plurals, which in Russian
  have three forms that a format directive cannot tell apart, are written as "what is counted: number" so
  that every number reads right. The catalog was written by the assistant and has not been read by a native
  speaker: expect to correct some of it. The lint and the tests ask of Russian what they ask of Portuguese,
  including that no English text reaches the screen in any of the windows.
- **Export as JPEG.** More > Export as JPEG... saves what the canvas shows, the rectangle where the user sees
  the grid and the drawing, as a `.jpg` file: at the view they have, at the sharpness of the display (a
  screen that doubles the pixels gives a picture of twice the size), and without the panels, the
  selection, the tool in use or the cursor. A window says it worked, with the size of the file and of the
  picture, or why it did not. The encoder is written in Lisp (baseline JPEG, quality 90, Huffman tables made
  for each picture), so the program still needs no library; it was checked against ImageMagick and a test
  decoder, and codes 2560 x 1440 in about half a second.
- **A start screen.** When the program starts without a drawing it opens a window in the middle of the
  screen with the recent drawings (those that still exist, up to five, the latest first, each with the
  name of its folder), **New drawing**, **Open a drawing...**, **Run a script...** and **Quit**. It is a
  list of choices like the menus are: arrows, Home, End and Enter, or the pointer; the rest of the window
  ignores the pointer and the keys while it is open, and Escape goes on with the empty drawing. A recent
  drawing that cannot be opened says why and leaves the screen open. Starting with a file, a demo and the
  tests skip it.
- DXF import reads more of what real files have: **splines** (B-splines and rational ones, from their
  knots, control points and weights; or through their fit points) become polylines of short straight
  segments, **leaders** become lines without their arrowheads, **3D faces** become their outlines
  flattened onto the plane, and **traces** are read as filled shapes are. A test file written the way
  AutoCAD 2000 writes one (classes, tables with handles and subclass markers, blocks, paper space,
  objects) imports whole.

## [1.1.0] - 2026-10-14

### Added

- **The console and the script language.** `F2` (or More > Console) opens a console under the canvas
  where a Lisp expression is typed and run against the drawing. The commands are Lisp functions:
  `line rect rect-size circle arc ellipse polygon pline text dimension` draw, `move copy erase edit
  move-to-layer` change, `layer with-layer layers entities entity-count extents info` look and
  arrange, `select select-all deselect selected fit` act on the window, `display-unit grid-spacing`
  set the drawing, `say` and `help` talk; all of Common Lisp is there too. Numbers are millimetres
  (`mm cm m inch foot` convert) and angles degrees (`(rad x)` for radians). Up and Down recall the
  commands run, Page Up, Page Down and the wheel scroll the output, an unfinished expression waits for
  its next lines, `Ctrl+L` clears. A command is one step of undo and is taken back whole when a step
  fails, when it takes more than 10 seconds (60 for a script file) or when it makes more than two
  million changes; what it prints is cut after 200,000 characters while it is written; reading
  evaluates nothing (`#.` is refused) and numbers are double-floats.
- **Run script and Export as script** in the More menu. *Run script* chooses a `.lisp` file in the file
  dialog, reads and counts it, and asks first, saying that a script is a program and showing how it
  begins; it runs only after the answer, as one command that can be undone. *Export as script* writes
  the drawing as the commands that draw it again, bit for bit, with layers (colors, hidden, locked) and
  every entity on its layer. `examples/house-plan.lisp` and `examples/rosette.lisp` are scripts that
  come with the program and are run by a test.
- **DXF import and export.** The More button of the title bar (the three dots) lists Import DXF
  (`Ctrl+Shift+I`) and Export DXF (`Ctrl+Shift+E`), which use the file dialog. *Import* reads an ASCII
  DXF of any version (UTF-8, or the Windows Western code page that files from Brazil are often in) as a
  new drawing, not saved yet: the layers (name, color, switched off, locked), lines, circles, arcs,
  polylines (old and lightweight, their arc segments cut into short straight ones), texts, multiline
  texts (one text for each line), ellipses (a part of one becomes a polyline), filled triangles and
  quadrilaterals (as outlines), blocks (`INSERT` is expanded, with scale, rotation, mirroring, arrays and
  blocks inside blocks) and dimensions (as the lines, arrowheads and text their block draws). A file that
  does not say its unit is followed by the question of what it is; a drawing far from the origin
  (survey coordinates) is moved to it, and a note says so. Entities in paper space are left out, and so is
  what the program cannot draw (splines, hatches, points, leaders, images, 3D), each counted by type in a
  report that stays on the screen until read and goes to the log. *Export* writes a DXF of version R12
  (AC1009), which everything reads: lines, circles, arcs, texts and polylines as themselves; rectangles,
  regular polygons and ellipses as closed polylines; dimensions as their lines, arrowheads and text;
  layers with their state and the nearest of the 256 colors of AutoCAD; text in ASCII, with the other
  characters as `\U+hhhh`. The writing says what changed shape. Neither changes the file of the drawing.
- **Grips.** Select one entity and handles appear on it: drag the end of a line, a corner of a
  rectangle, a point on a circle (the radius), the vertices of a polyline, the start, the end or the
  middle of an arc, the ends of the axes of an ellipse, a vertex of a polygon, the end of a text (its
  angle), the points and the line of a dimension. A circle handle in the middle moves the whole
  entity. The entity as it would become is previewed while you drag, the drag follows the grid and
  object snaps, Escape abandons it, and releasing it is one step of undo. A drag that would make
  the entity invalid changes nothing and says why. Polylines of more than 1000 vertices have handles
  on the first 1000 only.
- **New drawing types: arc, ellipse and regular polygon.** The arc takes its center, its start and
  a direction for its end (counter-clockwise); the ellipse takes its center, the end of its first axis
  and the other half-axis; the polygon takes its center and a vertex, and Up and Down set the number of
  sides (3 to 1000, kept for the next one). Keys `A`, `E` and `N`, listed in the draw menu. Each is
  edited in the inspector (angles in degrees), snaps to its center, its ends or its vertices, is picked
  by its outline, taken by crossing selection, copied and pasted, and stored in `.bcad` version 2.
- **Text at an angle.** A text can be turned to any angle: drag from the insertion point with the
  text tool (the angle snaps to multiples of 15 degrees when it is within 3 of one, and the text is
  previewed while dragging), or edit the Angle field of the inspector, in degrees. The text is
  drawn, picked and selected by crossing in its rotated box.
- **`.bcad` format version 2, and migration.** Version 2 stores the angle of a text. Version 1
  files still open: the format table records what each version added, with the value an older file
  gets, so a version 1 file is read as the version 2 file that says angle 0, and is written back as
  version 2. A version 1 file is not allowed to have what version 2 added. A version 1 fixture and a
  migration suite keep this honest.
- **Crossing selection.** Dragging on empty space from right to left selects what the rectangle
  touches (drawn dashed and green); from left to right it selects what lies inside, as before
  (solid). A diagonal line is taken only when it really passes through the rectangle, and a circle
  or a rectangle that holds the whole rectangle, without its outline reaching it, is not.
- **Copy, cut and paste of entities.** `Ctrl+C`, `Ctrl+X` and `Ctrl+V` copy, cut and paste the
  selection; `Ctrl+Shift+V` pastes in place. A paste is centered on the pointer (or on the view when
  the pointer is not over the canvas), becomes the selection so it can be moved at once, and is one
  step of undo. What is copied is the text of a small drawing in the `.bcad` format, on the system
  clipboard, so it pastes into another window of the program; it goes onto the layer of the same name
  when there is one that is visible and unlocked, and onto the current layer otherwise. Anything else
  on the clipboard is refused: the pasted text is read by the strict reader, which evaluates nothing.
- **Languages.** The interface speaks English (US), always the default, and Brazilian Portuguese. The
  globe button in the title bar opens a menu of languages; the choice is kept between runs, and
  `--language pt-BR` or `BORBA_CAD_LANGUAGE=pt-BR` override it. The English text of the program is the
  key of each message, so a text that has no translation shows in English; a catalog is a UTF-8
  data file (`src/i18n/pt-br.catalog`), and the lint fails when a text of the program has no
  translation in it or the catalog keeps one the program does not use. Messages with plurals,
  tool prompts, property names, errors and the file dialog are translated; the messages about the
  internal structure of a damaged `.bcad` file, the performance overlay, the log and the
  command-line help stay in English.
- **File dialog.** Open and Save As are one friendly screen: folders and drawings with size and
  date, places (home, documents, desktop, downloads, sample drawings), recent drawings, a clickable
  folder trail with a path field, live search, an All files switch, new folder, overwrite
  confirmation, keyboard navigation, and errors shown in the dialog with the dialog kept open.
- **Double-click distribution.** `make dist` builds a Linux folder and archive with the program, the
  libraries it needs (SDL2, FreeType and what they need), the fonts, the examples, the icon, a
  desktop launcher and install scripts; `make dist-check` verifies it in a clean environment. A
  Windows script, an MSYS2 target and a CI workflow are written but have never been run on Windows;
  the system-specific code is guarded and has portable fallbacks that the tests compare with the
  POSIX ones.
- The program takes a drawing as an argument (`borba-cad plan.bcad`), the standalone executable keeps
  its log and recent drawings in the user's state directory and finds its fonts and examples beside
  itself, and the window announces its application id so desktops show the icon.
- Drag a drawing onto the window to open it (after the usual question about unsaved changes).
- Page Up and Page Down keys; options `--state-dir` and `--drawings-dir`.
- A drawing with nothing in it says how to begin: a card over the canvas with the first tools and keys.
- Text fields take `Ctrl+A` (select all), `Ctrl+C`, `Ctrl+X` and `Ctrl+V` through the system clipboard,
  in the file dialog, the prompts and the inspector; pasted lines become one line.
- Typed lengths accept thousands separators in both conventions: `1.250,5` and `1,250.5` are 1250.5,
  `1.250.000` is a million and a quarter; `1,5` and `1.5` are still 1.5.

### Changed

- The colors that new layers are given in turn are one palette in the domain, used by the layers
  panel and by the `layer` command, so both give a new layer the same color. The lint knows that a
  function defined by `flet` or `labels` is not the global function of the same name, and that the
  functions of SBCL's own packages are not the project's.
- **One button for the drawing tools.** The palette holds the select tool and a Draw button; the
  button opens a list of the drawing tools (line, polyline, rectangle, circle, text, dimension) with
  their keys, and choosing one goes on as before. The letters L, R, C, P, T and D are unchanged. The
  list is a reusable drop-down menu: the mouse or the arrows and Enter choose, Escape or a click
  elsewhere closes it, and the rest of the window ignores the pointer while it is open.
- **New interface.** A dark theme with a teal accent, a title bar (brand mark, file and history
  buttons, document name with an unsaved-changes dot, performance toggle), a tool palette on the left
  with tooltips that show each tool's key, the layers and the properties as cards on the right, and a
  status bar with chips for the grid, the snaps, the display unit and zoom to fit. Buttons, rows,
  fields and dialogs are rounded, with soft shadows; icons and the brand mark are drawn from vector
  shapes, so they stay sharp at any size.
- Interface text is proportional (Noto Sans) and measured from the real glyph advances; drawing text
  keeps the fixed grid of the geometry module. Long text typed into a field scrolls with the caret
  instead of running out of the field, and long dialog messages wrap.

### Fixed

- An unexpected error in the program (a bug, not something the user did) while handling an event or
  drawing a frame ended the session and lost the unsaved drawing. It is now reported in the status
  bar, logged with its call stack (`app.internal-error`), and the program goes on; a frame that
  cannot be drawn three times in a row still ends it, so a failure that repeats cannot fill the log.
  `BORBA_CAD_DEBUG=1` leaves the error to the debugger as before.
- Messages in the status bar (Saved..., Layer created...) stayed there until another one replaced
  them, which hid the hint under the pointer and the tool's prompt; they now go after six seconds
  (errors after twelve).
- A layer added or chosen out of sight in a long list is scrolled into view.
- Switching to another window while Space was held, or in the middle of a pan or a drag, left the
  program thinking Space was still down and the drag still going (the release arrives in the other
  window); losing the keyboard now lets go of both, and the pointer leaving the window clears its
  position from the status bar.
- A folder with tens of thousands of files no longer makes the file dialog filter it again for
  every widget of every frame (20,000 files: 11 ms per frame, now 1 ms).

- Saving: a name without an extension is saved as `name.bcad`; blanks, one pair of quotes and
  `file://` addresses around a pasted path are understood instead of creating folders named `"`,
  `file:` or a blank in the current directory; saving over a symbolic link updates the file it
  points to instead of replacing the link; a replaced file keeps its permissions; saving never
  creates folders; saving over a different existing file asks first.
- Opening: a name without an extension finds the `.bcad` file; a folder, a missing file, a device or
  a blank name get a plain explanation instead of a Lisp stream error.
- The Save As and Open dialogs start in the documents folder of the desktop (`Documentos` on a
  Portuguese desktop) instead of a path relative to wherever the program was started.
- Messages report sizes in KB and MB.
- A length that could not be read was reported without the text that was typed (the message said
  `Invalid input ""` for `abc`).
- The outline of a rounded rectangle with a small corner radius was stitched with the wrong
  neighbours and drew a filled, broken shape.

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

- Tried on Linux x86_64 and SBCL only; no `LICENSE` file (the author has not chosen one).
- Hitches caused by garbage collection in long sessions with large drawings are not characterized
  (`docs/performance.md` §9).
- A drawing of 100,000 entities with everything in view draws at about 32 frames per second.
