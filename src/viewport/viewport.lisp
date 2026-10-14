(in-package #:borba-cad/viewport)

;;;; Coordinate systems.
;;;;   World:  millimetres, right-handed, +y up.
;;;;   Screen: logical window pixels, origin at the top-left of the window, +y down.
;;;; The canvas is a rectangle of the window (origin-x, origin-y, width, height). The transform is
;;;; screen = canvas-origin + canvas-size / 2 + (world - center) * scale, with y flipped. The
;;;; center-relative form avoids the cancellation that an absolute pan offset would cause at
;;;; large coordinates. Framebuffer pixels = logical pixels * display scale, a rendering
;;;; concern handled by the platform; nothing here depends on it.

(defconstant +minimum-scale+ 1d-4
  "Smallest zoom, in pixels per millimetre: one pixel is 10 m, so the whole coordinate range
(20 km across) fits in about 2000 pixels.")

(defconstant +maximum-scale+ 1d3
  "Largest zoom, in pixels per millimetre: one pixel is 1 micrometre, the drawing resolution.")

(defconstant +default-visible-width+ 20000d0
  "World width in millimetres shown by the default viewport (20 m).")

(defconstant +fit-margin-pixels+ 48d0
  "Empty border left around the content by FIT-VIEWPORT-TO-BOUNDS.")

(defstruct (world-point (:constructor make-world-point (x
                                                        y)))
  "A position in world coordinates.

X: millimetres along +x.
Y: millimetres along +y (up)."
  (x 0d0 :type double-float :read-only t)
  (y 0d0 :type double-float :read-only t))

(defstruct (screen-point (:constructor make-screen-point (x
                                                          y)))
  "A position in screen coordinates.

X: logical pixels from the left edge of the canvas.
Y: logical pixels from the top edge of the canvas (+y down)."
  (x 0d0 :type double-float :read-only t)
  (y 0d0 :type double-float :read-only t))

(defstruct (viewport (:constructor %make-viewport (center-x
                                                   center-y
                                                   scale
                                                   width
                                                   height
                                                   origin-x
                                                   origin-y))
                     (:copier nil))
  "A view of the drawing. Immutable: every operation returns a new viewport.

CENTER-X: world x shown at the center of the canvas.
CENTER-Y: world y shown at the center of the canvas.
SCALE: pixels per millimetre, between +MINIMUM-SCALE+ and +MAXIMUM-SCALE+.
WIDTH: canvas width in logical pixels.
HEIGHT: canvas height in logical pixels.
ORIGIN-X: window x of the canvas's left edge.
ORIGIN-Y: window y of the canvas's top edge."
  (center-x 0d0 :type double-float :read-only t)
  (center-y 0d0 :type double-float :read-only t)
  (scale 0.1d0 :type double-float :read-only t)
  (width 1d0 :type double-float :read-only t)
  (height 1d0 :type double-float :read-only t)
  (origin-x 0d0 :type double-float :read-only t)
  (origin-y 0d0 :type double-float :read-only t))

(defun clamp-scale (scale)
  "Return SCALE limited to the allowed zoom range.

SCALE: pixels per millimetre."
  (declare (type double-float scale))
  (max +minimum-scale+ (min +maximum-scale+ scale)))

(defun make-viewport (&key center-x
                           center-y
                           scale
                           width
                           height
                           (origin-x 0d0)
                           (origin-y 0d0))
  "Create a viewport; the scale is clamped to the allowed range and the canvas size must be
positive.

CENTER-X: world x at the canvas center, in millimetres.
CENTER-Y: world y at the canvas center, in millimetres.
SCALE: pixels per millimetre.
WIDTH: canvas width in logical pixels.
HEIGHT: canvas height in logical pixels.
ORIGIN-X: window x of the canvas's left edge.
ORIGIN-Y: window y of the canvas's top edge."
  (check-type center-x double-float)
  (check-type center-y double-float)
  (check-type scale double-float)
  (check-type width (double-float (0d0)))
  (check-type height (double-float (0d0)))
  (check-type origin-x double-float)
  (check-type origin-y double-float)
  (%make-viewport center-x
                  center-y
                  (clamp-scale scale)
                  width
                  height
                  origin-x
                  origin-y))

(defun default-viewport (width
                         height
                         &key (origin-x 0d0)
                              (origin-y 0d0))
  "Return the viewport shown for an empty drawing: origin at the center, 20 m across.

WIDTH: canvas width in logical pixels.
HEIGHT: canvas height in logical pixels.
ORIGIN-X: window x of the canvas's left edge.
ORIGIN-Y: window y of the canvas's top edge."
  (make-viewport :center-x 0d0
                 :center-y 0d0
                 :scale (/ width +default-visible-width+)
                 :width width
                 :height height
                 :origin-x origin-x
                 :origin-y origin-y))

(declaim (inline world->screen screen->world))
(defun world->screen (viewport
                      world-x
                      world-y)
  "Convert a world position to screen pixels; returns two values.

VIEWPORT: the viewport.
WORLD-X: x in millimetres.
WORLD-Y: y in millimetres."
  (declare (type double-float world-x world-y))
  (let ((scale (viewport-scale viewport)))
    (values (+ (viewport-origin-x viewport)
               (* 0.5d0 (viewport-width viewport))
               (* (- world-x (viewport-center-x viewport)) scale))
            (- (+ (viewport-origin-y viewport)
                  (* 0.5d0 (viewport-height viewport)))
               (* (- world-y (viewport-center-y viewport)) scale)))))

(defun screen->world (viewport
                      screen-x
                      screen-y)
  "Convert a screen position in pixels to world millimetres; returns two values.

VIEWPORT: the viewport.
SCREEN-X: x in logical pixels.
SCREEN-Y: y in logical pixels (down)."
  (declare (type double-float screen-x screen-y))
  (let ((scale (viewport-scale viewport)))
    (values (+ (viewport-center-x viewport)
               (/ (- screen-x (viewport-origin-x viewport) (* 0.5d0 (viewport-width viewport)))
                  scale))
            (+ (viewport-center-y viewport)
               (/ (- (+ (viewport-origin-y viewport) (* 0.5d0 (viewport-height viewport)))
                     screen-y)
                  scale)))))

(defun world-point->screen (viewport
                            point)
  "Convert a typed world point to a typed screen point.

VIEWPORT: the viewport.
POINT: a world-point."
  (multiple-value-bind (x y)
      (world->screen viewport
                     (world-point-x point)
                     (world-point-y point))
    (make-screen-point x y)))

(defun screen-point->world (viewport
                            point)
  "Convert a typed screen point to a typed world point.

VIEWPORT: the viewport.
POINT: a screen-point."
  (multiple-value-bind (x y)
      (screen->world viewport
                     (screen-point-x point)
                     (screen-point-y point))
    (make-world-point x y)))

(declaim (inline world-length->pixels pixels->world-length))
(defun world-length->pixels (viewport
                             length)
  "Convert a world length to a length in pixels.

VIEWPORT: the viewport.
LENGTH: millimetres."
  (declare (type double-float length))
  (* length (viewport-scale viewport)))

(defun pixels->world-length (viewport
                             pixels)
  "Convert a length in pixels to a world length.

VIEWPORT: the viewport.
PIXELS: logical pixels."
  (declare (type double-float pixels))
  (/ pixels (viewport-scale viewport)))

(defun resize-viewport (viewport
                        width
                        height
                        &key (origin-x (viewport-origin-x viewport))
                             (origin-y (viewport-origin-y viewport)))
  "Return a viewport with a new canvas rectangle and the same center and scale.

VIEWPORT: the viewport.
WIDTH: new canvas width in logical pixels.
HEIGHT: new canvas height in logical pixels.
ORIGIN-X: new window x of the canvas's left edge.
ORIGIN-Y: new window y of the canvas's top edge."
  (make-viewport :center-x (viewport-center-x viewport)
                 :center-y (viewport-center-y viewport)
                 :scale (viewport-scale viewport)
                 :width width
                 :height height
                 :origin-x origin-x
                 :origin-y origin-y))

(defun pan-viewport (viewport
                     delta-x
                     delta-y)
  "Return the viewport after dragging the content by (DELTA-X, DELTA-Y) pixels: a point that
was at screen position P is afterwards at P + (DELTA-X, DELTA-Y).

VIEWPORT: the viewport.
DELTA-X: pixels the content moves right.
DELTA-Y: pixels the content moves down."
  (declare (type double-float delta-x delta-y))
  (let ((scale (viewport-scale viewport)))
    (make-viewport :center-x (- (viewport-center-x viewport) (/ delta-x scale))
                   :center-y (+ (viewport-center-y viewport) (/ delta-y scale))
                   :scale scale
                   :width (viewport-width viewport)
                   :height (viewport-height viewport)
                   :origin-x (viewport-origin-x viewport)
                   :origin-y (viewport-origin-y viewport))))

(defun zoom-viewport-at (viewport
                         factor
                         screen-x
                         screen-y)
  "Return the viewport zoomed by FACTOR around a screen position: the world point under
(SCREEN-X, SCREEN-Y) stays under it. The scale is clamped, so zooming past a limit keeps the
anchor fixed at the limit scale.

VIEWPORT: the viewport.
FACTOR: multiplier of the scale, above 1 to zoom in.
SCREEN-X: anchor x in logical pixels.
SCREEN-Y: anchor y in logical pixels (down)."
  (declare (type double-float factor screen-x screen-y))
  (multiple-value-bind (anchor-x anchor-y)
      (screen->world viewport
                     screen-x
                     screen-y)
    (let ((scale (clamp-scale (* (viewport-scale viewport) factor))))
      (make-viewport :center-x (- anchor-x
                                  (/ (- screen-x (viewport-origin-x viewport) (* 0.5d0 (viewport-width viewport)))
                                     scale))
                     :center-y (+ anchor-y
                                  (/ (- screen-y (viewport-origin-y viewport) (* 0.5d0 (viewport-height viewport)))
                                     scale))
                     :scale scale
                     :width (viewport-width viewport)
                     :height (viewport-height viewport)
                     :origin-x (viewport-origin-x viewport)
                     :origin-y (viewport-origin-y viewport)))))

(defun fit-viewport-to-bounds (viewport
                               bounds
                               &key (margin-pixels +fit-margin-pixels+))
  "Return the viewport that shows BOUNDS centered with a margin around them (zoom extents).
Bounds smaller than the drawing resolution in both directions are treated as that size.

VIEWPORT: the viewport supplying the canvas size.
BOUNDS: world bounds to show.
MARGIN-PIXELS: empty border kept on every side."
  (let* ((width (max (geo:bounds-width bounds) base:+drawing-resolution+))
         (height (max (geo:bounds-height bounds) base:+drawing-resolution+))
         (usable-width (max 1d0 (- (viewport-width viewport) (* 2 margin-pixels))))
         (usable-height (max 1d0 (- (viewport-height viewport) (* 2 margin-pixels)))))
    (make-viewport :center-x (geo:bounds-center-x bounds)
                   :center-y (geo:bounds-center-y bounds)
                   :scale (min (/ usable-width width) (/ usable-height height))
                   :width (viewport-width viewport)
                   :height (viewport-height viewport)
                   :origin-x (viewport-origin-x viewport)
                   :origin-y (viewport-origin-y viewport))))

(defun viewport-visible-bounds (viewport
                                &optional (margin-pixels 0d0))
  "Return the world bounds covered by the canvas, grown by MARGIN-PIXELS on every side.

VIEWPORT: the viewport.
MARGIN-PIXELS: extra border in pixels, used so geometry just outside the canvas still draws."
  (declare (type double-float margin-pixels))
  (multiple-value-bind (left top)
      (screen->world viewport
                     (- (viewport-origin-x viewport) margin-pixels)
                     (- (viewport-origin-y viewport) margin-pixels))
    (multiple-value-bind (right bottom)
        (screen->world viewport
                       (+ (viewport-origin-x viewport) (viewport-width viewport) margin-pixels)
                       (+ (viewport-origin-y viewport) (viewport-height viewport) margin-pixels))
      (geo:bounds-from-corners left
                               top
                               right
                               bottom))))
