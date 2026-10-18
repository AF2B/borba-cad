# Borba CAD — Geometry

How the program represents space, which tolerances it uses and why, and what each geometric
operation guarantees. Every statement here is backed by the code it names or by a test; the tests
are listed in §10. The numbers come from `src/base/tolerances.lisp`, `src/geometry/`,
`src/viewport/` and `src/domain/`.

## 1. Coordinate systems

| Space | Unit | Axes | Where it lives |
|---|---|---|---|
| World | millimetre, `double-float` | right-handed, +x right, +y up; angles in radians, counter-clockwise from +x | `geometry`, `domain`, `persistence` |
| Screen | logical window pixel | origin at the top-left of the window, +y down | `viewport`, `scene`, `ui`, `app` |
| Framebuffer | physical pixel = logical pixel × display scale | as screen | `platform` only |

World and screen points are distinct types (`world-point`, `screen-point`) so they cannot be mixed
silently. The conversion exists in one place, `src/viewport/viewport.lisp`; `geometry` and `domain`
contain no screen or pixel concept. The canvas is a rectangle of the window, so every viewport
carries the window position of its top-left corner (`origin-x`, `origin-y`).

SBCL's float traps (overflow, invalid, divide by zero) stay enabled in all Lisp code, so a NaN or
an infinity in geometry signals instead of propagating. Nothing in the SDL/OpenGL calls trapped
during the spikes, so the platform layer does not mask them.

## 2. Units

The canonical unit is the millimetre. A typical building dimension such as 150 mm is exact in
binary floating point; 0.15 m is not. The display unit (mm, cm, m) is a per-drawing setting and
changes only what the user sees and types: conversion happens at the user-interface boundary in
`src/base/units.lisp`, the only file that contains unit factors.

| Unit | Abbreviation | mm per unit | Default decimals |
|---|---|---|---|
| millimetre | mm | 1 | 0 |
| centimetre | cm | 10 | 1 |
| metre | m | 1000 | 2 |

- `parse-length` accepts a number with an optional sign, either `.` or `,` as the decimal
  separator, and an optional unit suffix (`mm`, `cm` or `m`, any case); the number is parsed as an
  exact rational and converted once, so `0.1` typed in metres is exactly 100 mm. At most 32 digits
  are accepted and exponent notation (`1e5`) is not. Anything else signals `invalid-input` with a
  message that says what was wrong and what is accepted.
- `format-decimal` rounds half away from zero on the exact binary value, never prints exponent
  notation and never prints a negative zero.
- A dimension stores its own unit and precision (0 to 6 decimals), so its text does not change
  when the drawing's display unit does.

## 3. Numeric policy

| Constant | Value | Meaning |
|---|---|---|
| `+maximum-coordinate-magnitude+` | 1e7 mm (10 km) | Largest absolute coordinate that may exist. |
| `+drawing-resolution+` | 1e-3 mm (1 µm) | Smallest difference a drafter can mean; also the finest zoom (one pixel per resolution step). |
| `+length-tolerance+` | 1e-6 mm (1 nm) | Lengths at or below it count as zero. |
| `+maximum-length+` | 3e7 mm | Largest accepted length value. |

Why these values. A double has 53 significant bits, so the gap between adjacent doubles at 1e7 is
2⁻²⁹ mm ≈ 1.9e-9 mm: the worst-case representation error of any stored coordinate. The length
tolerance is about 500 times that, so rounding noise can never make a valid segment degenerate,
and 1000 times smaller than the drawing resolution, so no intentional feature is ever treated as
degenerate. The maximum length is above the diagonal of the square of valid coordinates (2.83e7
mm), so every distance between two valid points fits, and far from overflow for any product or sum
computed from lengths. `tolerance-sits-between-representation-noise-and-drawing-resolution`
checks the first relation numerically; `positive-length-predicate-uses-the-tolerance-as-an-exclusive-bound`
checks that a length equal to the tolerance is rejected and twice the tolerance is accepted.

Rules that follow from the policy:

- Constructors reject coordinates that are not finite doubles within ±1e7 mm, and lengths (radius,
  width, height, text height) that are not above the length tolerance. The error is a typed
  condition (`invalid-entity`) naming the entity kind and the reason.
- Pixel-based distances (pick radius 6 px, snap radius 12 px, drag threshold 4 px, minimum grid
  spacing 8 px, culling margin 4 px, circle chordal error 0.25 px) are user-interface constants in
  `app`, `viewport` and `scene`. They are converted to world lengths at the current zoom before
  they meet geometry, and are never mixed with the tolerances above.
- Floating-point equality is used only where its meaning is exact and documented: touching
  rectangles intersect, a point on a boundary is contained, a tie in snapping goes toward +∞.

## 4. The kernel (`src/geometry/`)

All functions take raw doubles and return doubles; none allocates in its hot path.

| Operation | Definition and behavior |
|---|---|
| Bounds | `bounds` is an axis-aligned rectangle (min and max, min ≤ max). `bounds-from-corners` normalizes corners given in any order. Union, intersection (touching counts), containment (touching counts), `expand-bounds` (a negative margin shrinks). |
| `distance`, `segment-length` | Euclidean distance. |
| `normalize-angle`, `segment-angle` | Angle reduced to [0, 2π); a tiny negative angle that rounds to 2π returns 0. `segment-angle` is `atan2(dy, dx)` normalized. |
| `snap-coordinate` | The multiple of the spacing nearest to the value; ties go toward +∞ (`floor(v/s + 1/2) · s`). The result is a grid multiple within half a step of the input. |
| `closest-point-on-segment` | Projection onto the segment with the parameter clamped to [0, 1]; returns x, y and the parameter. A zero-length segment returns its single point with parameter 0 (no division by zero). |
| `point-segment-distance` | Distance to the segment, not to its infinite line. |
| `point-circle-outline-distance` | `abs(distance(p, center) − r)`: zero on the circle, `r` at the center. |
| `point-rectangle-outline-distance` | From outside: the Euclidean distance to the rectangle (nearest edge or corner). From inside: the distance to the nearest edge. |
| `point-polyline-distance` | Minimum over the segments; the closing segment counts only when `closed` and the polyline has at least 3 vertices. |
| `bounds-near-point-p` | Cheap pre-filter used by picking: true when the point is within the radius of the bounds along both axes. Every shape inside the bounds that is closer than the radius passes it. |
| `clip-segment-to-bounds` | Liang–Barsky. Returns the clipped endpoints, or NIL when no part of the segment lies inside. It runs in double precision before any coordinate becomes a single-float vertex, so a segment with a far-away endpoint keeps its precision on screen. |
| Arcs, ellipses, regular polygons (`curves.lisp`) | `arc-bounds` (the ends and the axis extremes the arc passes through), `point-arc-distance` (to the circle where the point lies in the arc's direction, to the nearer end otherwise), `arc-touches-bounds-p` (an end inside the window, or the circle crossing a window edge within the sweep), `ellipse-bounds` (half-extents `sqrt(a²cos²θ + b²sin²θ)`), `point-ellipse-distance` (D. Eberly's bisection for the nearest point, exact to the precision of a double), `ellipse-perimeter` (Ramanujan's second approximation), `ellipse-touches-bounds-p` (on the inscribed 128-gon), `regular-polygon-vertices`. Tested against dense sampling of random arcs and ellipses. |
| `segment-touches-bounds-p`, `polyline-touches-bounds-p`, `circle-outline-touches-bounds-p`, `oriented-box-touches-bounds-p` | Whether the outline of a shape has a point inside a window or on its edge (a filled rotated box is filled, so a window inside it counts). The circle test compares the radius with the nearest and the farthest point of the window. Tested against dense sampling of random segments and circles. |
| Measures | Circle circumference `2πr` and area `πr²`; rectangle area and perimeter; polyline length (the closing segment included when closed) and bounds. |
| `contour-area`, `angle-between-rays` (`measure.lisp`) | The area enclosed by a closed contour of vertices, by the shoelace formula (orientation does not matter; a contour that crosses itself gives the difference of its loops; fewer than three vertices enclose nothing). The angle at a vertex between the rays toward two points, `atan2(|a × b|, a · b)`, in [0, π]; NIL when a point lies at the vertex. The measure tools of the application use them. |

## 5. Text and dimension geometry

Text uses a fixed-advance model so that its bounds depend only on the number of characters and the
text height, never on a font file (decision A6):

| Quantity | Value |
|---|---|
| Advance of one character | 0.6 × text height (one cell per code point) |
| Nominal box above the baseline | 0.95 × text height (tall enough for accented capitals) |
| Nominal box below the baseline | 0.25 × text height |

The monospace fonts used by the renderer advance exactly 0.6 em (verified for Noto Sans Mono), so
the renderer places every glyph on the same grid and the domain bounds match what is drawn.
`oriented-box-bounds` returns the axis-aligned bounds of that box rotated about its origin, and
`point-oriented-box-distance` the distance from a point to the filled rotated box; picking text
and dimensions uses them. A text has an angle (radians, counter-clockwise from +x, kept in
[0, 2π)) that turns its baseline about the insertion point, so its bounds, its picking and its
crossing test all use the rotated box, and a text turned 45 degrees is not picked in the empty
corner of its bounds. The text tool sets it by dragging from the insertion point (snapped to
multiples of 15 degrees within 3 degrees); the inspector edits it in degrees.

An aligned dimension measures the segment between two definition points P1 and P2. The dimension
line is parallel to it, displaced by a signed offset along the left normal (a negative offset
displaces to the right). Every size scales with the text height `h`, so a dimension keeps its
proportions at any drawing scale:

| Part | Rule |
|---|---|
| Arrowhead | length `1.0·h`, base width 0.3 of its length; filled triangle |
| Extension lines | start `0.4·h` from the definition point (or the offset itself when smaller), run `0.6·h` past the dimension line |
| Text | centered on the dimension line, baseline `0.35·h` above it, direction chosen so that it reads left to right (the direction flips when the line points leftward) |
| Short dimensions | when the measured length is below twice the arrow length the arrows sit outside the line, pointing inward |
| Measured value | derived from the two points and never stored; the displayed text is formatted in the dimension's own unit and precision |

The layout is a plain value (`dimension-layout`: 8 doubles of extension lines, 4 of the dimension
line, 12 of the arrowheads, the text anchor, angle and width, and the bounds of every part), so
`scene` draws it and `domain` measures it with the same numbers.

The radius, diameter and angle dimensions (`geometry/annotation.lisp`) share one layout value,
`annotation-layout`: a list of stroked segments, a list of arrowhead triangles, the text anchor, angle and width,
and the bounds of every part. The sizes are the same multiples of the text height `h` as above.

| Dimension | Layout |
|---|---|
| Radius | a leader from the center through the circle at the leader angle, continuing past it by a tail of `0.5·h`, the width of the text and `0.5·h`; an arrowhead with its tip on the circle pointing outward; the text on the tail, baseline `0.35·h` above it, running along it and flipped when the leader points leftward so that it reads left to right |
| Diameter | the line through the center at the leader angle, from circle to circle, with an arrowhead at each end pointing outward; the text centered above it |
| Angle | two extension lines along the arms, from `0.4·h` (or half the radius when smaller) from the vertex to `0.6·h` past the arc; the arc, in segments of at most four degrees, counter-clockwise from the first arm to the second; an arrowhead at each end of the arc, tangent to it; the text level, centered on the bisector, beyond the arc by `0.4·h` plus half of its extent along the bisector |
| Small dimensions | when the line or the arc is shorter than twice the arrow length the arrows sit outside it, pointing inward |

Picking and crossing use the segments and the filled box of the text, so the arc is picked on its chords
(within a millimetre of the true arc).

## 6. Entities and their invariants

Entities are immutable values built by validating constructors; a violation signals `invalid-entity`
and nothing is created. Every entity has a unique id, never reused in a drawing, and exactly one
layer. Stacking order is id order.

| Entity | Invariants beyond finite, in-range coordinates | Derived values shown by the inspector |
|---|---|---|
| Line | length > tolerance | length, angle |
| Rectangle (axis-aligned) | width and height > tolerance; corners given in any order are normalized | area, perimeter |
| Circle | radius > tolerance | diameter, circumference, area |
| Arc | radius > tolerance; start and end angles differ by more than the angle tolerance and by less than a whole turn (an arc turns counter-clockwise from the start to the end, and the angles are kept in [0, 2π)) | length, sweep |
| Ellipse | both semi-axes > tolerance; rotation of the first axis kept in [0, π) | perimeter (Ramanujan), area |
| Regular polygon | 3 to 1000 sides; radius (of the circle through the vertices) > tolerance; rotation of the first vertex kept in [0, 2π) | side length, perimeter, area |
| Polyline | at least 2 vertices (3 when closed), at most 100,000; no two consecutive vertices coincide | length, vertex count |
| Text | non-empty single line, at most 256 characters; height > tolerance; any angle, kept in [0, 2π) | width |
| Dimension | definition points farther apart than the tolerance; text height > tolerance; precision 0 to 6 | measured length |
| Radius or diameter dimension | radius > tolerance; text height > tolerance; precision 0 to 6; leader angle kept in [0, 2π); the text is `R` and the radius, or the diameter sign and twice the radius | measured length |
| Angle dimension | arm directions differ by more than the angle tolerance and by less than a whole turn (the angle is measured counter-clockwise from the first arm to the second); radius > tolerance; text height > tolerance; precision 0 to 6 | angle |

Rotating, mirroring and scaling (`src/domain/transforms.lisp`) are generic functions that return a new
entity of the same id and layer: `entity-rotated`, `entity-mirrored` and `entity-scaled`. A mirror reverses
the sense of an arc, so its start and end angles swap and it still turns counter-clockwise; a text is moved
but not turned back to front; a dimension negates its offset so it stays on the same side of what it
measures; a scale also scales the text heights and the dimension offset. A rectangle stays one only while
the result is axis-aligned; otherwise it becomes a closed polyline with the same id. The result goes
through the usual constructor, so a transformation that leaves the drawing limit or shrinks a length below
the tolerance signals `invalid-entity` and changes nothing.

Layers hold a unique non-empty name (at most 64 characters), an RGB color, and the flags visible
and locked. The default layer (id 0) always exists and cannot be deleted; a layer that holds
entities cannot be deleted either.

## 7. Picking, selection and snapping (`src/domain/picking.lisp`)

- **Pickable** means the entity's layer exists, is visible and is not locked.
- **Pick**: scan from the topmost entity down; the nearest pickable entity within the radius wins,
  and when two are at the same distance the one created last wins. The radius is 6 pixels
  converted to millimetres at the current zoom, so picking feels the same at every zoom level.
- **Window selection** (dragging from left to right) returns the pickable entities whose bounds lie
  entirely inside the window (touching counts), in stacking order.
- **Crossing selection** (dragging from right to left) returns the pickable entities that are
  inside the window or touch it, in stacking order: the window is first compared with the bounds of
  the entity, and then `entity-touches-bounds-p` asks the shape itself, so a diagonal line whose
  bounds overlap the window but which passes beside it is not taken, and a circle or a rectangle
  that holds the whole window without its outline reaching it is not taken either. A filled box
  (a text) is taken when its bounds overlap.
- **Grips** (`entity-grips`, `entity-with-grip`): each kind lists the handles that change it and
  answers what it becomes when a handle is dragged to a point, with the same id and layer, through its
  validating constructor; so a drag that would make it invalid signals `invalid-entity` and changes
  nothing. Line: both ends and the middle (moves it). Rectangle: the four corners, the opposite one
  staying put, and the center. Circle: four points of the outline (radius) and the center. Polyline:
  each vertex (the first 1000). Text: the insertion point (moves it) and the end of the baseline
  (angle). Dimension: the two definition points and the dimension line (offset). Arc: the two ends
  (angles), the middle (radius) and the center. Ellipse: the four ends of the axes and the center.
  Polygon: each vertex (radius and rotation) and the center. `grip-changes` builds the change set,
  checking that the entity can be edited.
- **Object snap** offers, within 12 pixels converted to millimetres, the endpoints (line ends,
  rectangle corners, polyline vertices, dimension definition points, a text insertion point) and centers
  that each entity lists (`entity-snap-points`), and the points that are found from the primitives each
  entity reduces to (`entity-primitives` in `src/domain/snapping.lisp`: segments, circles and arcs; an
  ellipse, a text and a dimension have none): the **midpoint** of a segment or an arc, the **quadrants** of
  a circle or the ones an arc covers, the **intersections** between the primitives of the entities near the
  pointer (at most 40 of them, so at most a few hundred pairs), and, when a tool has a last point (the
  anchor), the **perpendicular** feet (on a segment only when the foot is on it, on a circle the two points
  in line with the center) and the **tangent** points of a circle or an arc (from a point outside it). The
  crossings come from `src/geometry/intersect.lisp`, with a margin of 1e-9 on the parameters so that two
  segments that meet end to end cross. The nearest candidate wins; at equal distance (to 1e-9) the kind that
  comes first in `*object-snap-priority*` does: endpoint, intersection, center, midpoint, quadrant,
  perpendicular, tangent. **Grid snap** rounds to the drawing's own grid spacing, not to the displayed one.
  Object snap wins over grid snap; with both off the raw coordinates are returned. **Ortho** and **polar**
  (application, `constrain-point`) project the snapped point on the horizontal or vertical line through the
  anchor, or on the ray at the nearest multiple of 15 degrees, unless an object snap was taken.
- A move or an edit that would break an invariant is refused and leaves the drawing unchanged:
  change sets are validated completely before the first modification.

## 8. Viewport math (`src/viewport/`)

```
screen.x = origin.x + width/2  + (world.x − center.x) · scale
screen.y = origin.y + height/2 − (world.y − center.y) · scale
```

The center-relative form avoids the cancellation that an absolute pan offset would cause at large
coordinates: the subtraction `world − center` is exact enough for coordinates tens of kilometres
away, and the only rounding left is the final multiply and add.

- **Zoom limits**: 1e-4 to 1e3 pixels per millimetre. At the minimum one pixel is 10 m (the whole
  20 km coordinate range fits in about 2,000 pixels); at the maximum one pixel is 1 µm, the
  drawing resolution. A scale outside the range is clamped, never rejected.
- **Pan** by a drag of (dx, dy) pixels moves the content by exactly that many pixels.
- **Zoom at the cursor** multiplies the scale by the factor (1.2 per wheel notch) and recomputes the
  center so that the world point under the cursor stays under it. At a zoom limit the anchor still
  stays fixed.
- **Zoom extents** shows the bounds centered with a 48-pixel margin; bounds smaller than the drawing
  resolution in both directions are treated as that size, so a single point or an empty drawing
  never divides by zero.
- **Round trip**: converting world → screen → world stays within 1e-8 mm. The worst-case rounding
  error is `|world − center| · 2.2e-16`, at most 4.4e-9 mm for points up to 20 km from the center;
  the tolerance is about twice that.
- **Adaptive grid**: the displayed spacing is the drawing's spacing times the smallest multiplier of
  the ladder 1, 2, 5, 10, 20, 50, … that keeps the lines at least 8 pixels apart; every fifth line
  is a major line, and at most 4,000 lines are drawn per axis. The grid thins out when zooming out
  instead of turning into a solid fill.

## 9. From geometry to pixels (`src/scene/`)

- A circle is tessellated by its on-screen radius `r`: the segment count is `ceil(π / acos(1 − 0.25/r))`,
  which keeps the distance between the polygon and the true circle at or below 0.25 pixel, clamped
  to 12 to 4096 segments (a circle of 0.25 pixel or less uses 12).
- An entity whose bounds are smaller than one pixel is drawn as a one-pixel dash at its center;
  text smaller than 3 pixels as a baseline stroke; arrowheads shorter than 2 pixels are skipped.
- Segments are clipped to the canvas, grown by a 4-pixel margin so that lines do not end at the
  edge, in double precision, before conversion to single-float screen vertices.
- Entities outside the margin, on hidden layers or entirely culled are counted but not drawn; the
  scene result reports drawn and culled counts.

## 10. Evidence

Each operation has normal, edge, degenerate and invalid-input tests, and most have an independent
reference. The suites are `geometry`, `viewport`, `base`, `domain-entity` and `domain-drawing` in
`tests/`; all run with `make test`.

| Claim | Test |
|---|---|
| Tolerance relations | `tolerance-sits-between-representation-noise-and-drawing-resolution`, `positive-length-predicate-uses-the-tolerance-as-an-exclusive-bound`, `coordinate-predicate-covers-boundaries` |
| Units | `unit-conversions-round-trip`, `unit-factors-are-exact-for-common-building-dimensions`, `parse-length-accepts-units-signs-and-both-separators`, `parse-length-is-exact-for-decimal-text`, `format-decimal-rounds-half-away-from-zero-on-the-exact-value` |
| Distances against textbook formulas and brute force | `point-segment-distance-matches-the-textbook-formulas`, `point-segment-distance-agrees-with-brute-force-sampling`, `rectangle-outline-distance-agrees-with-the-four-edges`, `circle-outline-distance`, `polyline-distance-open-and-closed` |
| Degenerate input | `point-segment-distance-handles-degenerate-segments`, `fitting-handles-degenerate-bounds-without-dividing-by-zero` |
| Snapping | `snap-coordinate-rounds-to-the-nearest-multiple-with-ties-upward`, `snap-coordinate-property-result-is-a-grid-multiple-within-half-a-step` |
| Clipping | `clipping-analytic-cases`, `clipping-keeps-far-endpoints-precise`, `clipping-property-result-is-inside-collinear-and-complete` |
| Text and dimensions | `oriented-box-bounds-at-45-degrees-covers-the-rotated-corners`, `horizontal-dimension-layout-matches-hand-computed-values`, `short-dimension-puts-arrows-outside-pointing-inward`, `dimension-text-always-reads-left-to-right`, `dimension-layout-property-parts-are-consistent` |
| Transforms | `world-screen-round-trip-stays-within-the-documented-tolerance` (3,000 seeded random viewports across the whole zoom range and coordinates up to ±1e7 mm from the center), `zoom-keeps-the-world-point-under-the-cursor-fixed`, `panning-moves-the-content-by-exactly-the-dragged-pixels`, `zoom-scales-by-the-factor-and-respects-both-limits` |
| Grid | `grid-level-follows-the-1-2-5-ladder-when-zoomed-out`, `grid-level-property-spacing-is-wide-enough-and-minimal`, `visible-grid-range-covers-exactly-the-lines-inside-the-canvas` |

## 11. Known limits

- No splines or rotated rectangles; aligned linear dimensions only.
- The text box is a nominal model (fixed advance, 0.95 / 0.25 of the height), not the ink bounds of
  the glyphs, and assumes one cell per code point: combining characters and double-width characters
  are not laid out specially.
- Picking and window selection scan every entity with a bounds pre-filter; there is no spatial
  index. The cost is measured in [performance.md](performance.md).
