(in-package #:borba-cad/scene)

;;;; Shapes beyond lines and flat rectangles, built from triangles: thick strokes, discs, rings,
;;;; rounded rectangles, soft shadows, gradients and dashes. The user interface is drawn with them.
;;;; Edges are smoothed by the renderer's multisampling, so no feathering is done here.

(defconstant +minimum-shape-segments+ 8
  "Fewest segments of any round shape.")

(defconstant +maximum-shape-segments+ 96
  "Most segments of any round shape.")

(defconstant +shape-chordal-error-pixels+ 0.15f0
  "Largest distance between a round shape and the true curve, in pixels.")

(defconstant +shadow-layers+ 5
  "Translucent layers that make up one soft shadow.")

(defconstant +two-pi-single+ (* 2f0 (float pi 1f0))
  "A full turn in radians, as a single-float.")

(defun shape-segments (radius)
  "Return how many segments make a circle of RADIUS pixels look round, between the minimum and
the maximum.

RADIUS: radius in pixels."
  (let ((radius (float radius 1f0)))
    (if (<= radius +shape-chordal-error-pixels+)
        +minimum-shape-segments+
        (max +minimum-shape-segments+
             (min +maximum-shape-segments+
                  (ceiling (float pi 1f0)
                           (acos (- 1f0 (/ +shape-chordal-error-pixels+ radius)))))))))

(defun with-alpha (color
                   alpha)
  "Return COLOR with its opacity multiplied by ALPHA.

COLOR: an RGBA.
ALPHA: a factor from 0 to 1."
  (make-rgba (rgba-r color)
             (rgba-g color)
             (rgba-b color)
             (* alpha (rgba-a color))))

(defun add-thick-line (list
                       x1
                       y1
                       x2
                       y2
                       thickness
                       color)
  "Append a straight stroke of the given thickness as a quad. A zero-length stroke adds nothing.

LIST: a draw list.
X1: start x in pixels.
Y1: start y in pixels.
X2: end x in pixels.
Y2: end y in pixels.
THICKNESS: stroke width in pixels.
COLOR: an RGBA."
  (declare (type single-float x1 y1 x2 y2 thickness))
  (let* ((dx (- x2 x1))
         (dy (- y2 y1))
         (length (sqrt (+ (* dx dx) (* dy dy)))))
    (when (> length 1f-6)
      (let ((nx (* (/ (- dy) length) (* 0.5f0 thickness)))
            (ny (* (/ dx length) (* 0.5f0 thickness))))
        (add-triangle list
                      (+ x1 nx)
                      (+ y1 ny)
                      (- x1 nx)
                      (- y1 ny)
                      (- x2 nx)
                      (- y2 ny)
                      color)
        (add-triangle list
                      (+ x1 nx)
                      (+ y1 ny)
                      (- x2 nx)
                      (- y2 ny)
                      (+ x2 nx)
                      (+ y2 ny)
                      color)))))

(defun add-disc (list
                 center-x
                 center-y
                 radius
                 color)
  "Append a filled circle as a fan of triangles.

LIST: a draw list.
CENTER-X: center x in pixels.
CENTER-Y: center y in pixels.
RADIUS: radius in pixels.
COLOR: an RGBA."
  (declare (type single-float center-x center-y radius))
  (let ((segments (shape-segments radius)))
    (dotimes (index segments)
      (let ((first (* +two-pi-single+ (/ index (float segments 1f0))))
            (second (* +two-pi-single+ (/ (1+ index) (float segments 1f0)))))
        (add-triangle list
                      center-x
                      center-y
                      (+ center-x (* radius (cos first)))
                      (+ center-y (* radius (sin first)))
                      (+ center-x (* radius (cos second)))
                      (+ center-y (* radius (sin second)))
                      color)))))

(defun add-arc-stroke (list
                       center-x
                       center-y
                       radius
                       start-angle
                       end-angle
                       thickness
                       color)
  "Append a stroked arc: the part of a ring between two angles. Angles grow clockwise on screen,
because the y axis points down; a full turn is a ring.

LIST: a draw list.
CENTER-X: center x in pixels.
CENTER-Y: center y in pixels.
RADIUS: radius of the middle of the stroke, in pixels.
START-ANGLE: where the arc begins, in radians.
END-ANGLE: where it ends, in radians.
THICKNESS: stroke width in pixels.
COLOR: an RGBA."
  (declare (type single-float center-x center-y radius start-angle end-angle thickness))
  (let* ((inner (max 0f0 (- radius (* 0.5f0 thickness))))
         (outer (+ radius (* 0.5f0 thickness)))
         (sweep (- end-angle start-angle))
         (segments (max 2 (ceiling (* (shape-segments outer) (/ (abs sweep) +two-pi-single+))))))
    (dotimes (index segments)
      (let* ((a (+ start-angle (* sweep (/ index (float segments 1f0)))))
             (b (+ start-angle (* sweep (/ (1+ index) (float segments 1f0)))))
             (ax (cos a))
             (ay (sin a))
             (bx (cos b))
             (by (sin b)))
        (add-triangle list
                      (+ center-x (* inner ax))
                      (+ center-y (* inner ay))
                      (+ center-x (* outer ax))
                      (+ center-y (* outer ay))
                      (+ center-x (* outer bx))
                      (+ center-y (* outer by))
                      color)
        (add-triangle list
                      (+ center-x (* inner ax))
                      (+ center-y (* inner ay))
                      (+ center-x (* outer bx))
                      (+ center-y (* outer by))
                      (+ center-x (* inner bx))
                      (+ center-y (* inner by))
                      color)))))

(defun add-ring (list
                 center-x
                 center-y
                 radius
                 thickness
                 color)
  "Append a circle outline of the given thickness.

LIST: a draw list.
CENTER-X: center x in pixels.
CENTER-Y: center y in pixels.
RADIUS: radius of the middle of the stroke, in pixels.
THICKNESS: stroke width in pixels.
COLOR: an RGBA."
  (add-arc-stroke list
                  center-x
                  center-y
                  radius
                  0f0
                  +two-pi-single+
                  thickness
                  color))

(defun corner-steps (radius)
  "Return how many segments approximate one quarter-circle corner of RADIUS pixels.

RADIUS: corner radius in pixels."
  (max 1 (ceiling (shape-segments radius) 4)))

(defun rounded-rectangle-points (x
                                 y
                                 width
                                 height
                                 radius
                                 &key (steps (corner-steps radius)))
  "Return the outline of a rounded rectangle as a list of (x . y) points, clockwise from the end
of the top-left corner arc.

X: left edge.
Y: top edge.
WIDTH: width.
HEIGHT: height.
RADIUS: corner radius, limited to half the smaller side.
STEPS: segments per corner; outlines that must pair up point for point share it."
  (declare (type single-float x y width height radius)
           (type fixnum steps))
  (let* ((radius (max 0f0 (min radius (* 0.5f0 width) (* 0.5f0 height))))
         (right (+ x width))
         (bottom (+ y height))
         (half-turn (/ (float pi 1f0) 2))
         (points '()))
    (flet ((corner (center-x center-y start)
             (dotimes (step (1+ steps))
               (let ((angle (+ start (* half-turn (/ step (float steps 1f0))))))
                 (push (cons (+ center-x (* radius (cos angle)))
                             (+ center-y (* radius (sin angle))))
                       points)))))
      (corner (+ x radius) (+ y radius) (* 2 half-turn))
      (corner (- right radius) (+ y radius) (* 3 half-turn))
      (corner (- right radius) (- bottom radius) 0f0)
      (corner (+ x radius) (- bottom radius) half-turn))
    (nreverse points)))

(defun add-rounded-rectangle (list
                              x
                              y
                              width
                              height
                              radius
                              color)
  "Append a filled rectangle with rounded corners.

LIST: a draw list.
X: left edge.
Y: top edge.
WIDTH: width.
HEIGHT: height.
RADIUS: corner radius in pixels.
COLOR: an RGBA."
  (declare (type single-float x y width height radius))
  (let* ((points (rounded-rectangle-points x
                                           y
                                           width
                                           height
                                           radius))
         (center-x (+ x (* 0.5f0 width)))
         (center-y (+ y (* 0.5f0 height))))
    (loop for (first second) on (append points (list (first points)))
          while second
          do (add-triangle list
                           center-x
                           center-y
                           (car first)
                           (cdr first)
                           (car second)
                           (cdr second)
                           color))))

(defun add-rounded-rectangle-outline (list
                                      x
                                      y
                                      width
                                      height
                                      radius
                                      thickness
                                      color)
  "Append the outline of a rounded rectangle, with its stroke inside the given bounds.

LIST: a draw list.
X: left edge.
Y: top edge.
WIDTH: width.
HEIGHT: height.
RADIUS: corner radius in pixels.
THICKNESS: stroke width in pixels.
COLOR: an RGBA."
  (declare (type single-float x y width height radius thickness))
  (let* ((steps (corner-steps radius))
         (outer (rounded-rectangle-points x
                                          y
                                          width
                                          height
                                          radius
                                          :steps steps))
         (inner (rounded-rectangle-points (+ x thickness)
                                          (+ y thickness)
                                          (max 0f0 (- width (* 2 thickness)))
                                          (max 0f0 (- height (* 2 thickness)))
                                          (max 0f0 (- radius thickness))
                                          :steps steps)))
    (loop for (outer-first outer-second) on (append outer (list (first outer)))
          for (inner-first inner-second) on (append inner (list (first inner)))
          while (and outer-second inner-second)
          do (add-triangle list
                           (car outer-first)
                           (cdr outer-first)
                           (car outer-second)
                           (cdr outer-second)
                           (car inner-second)
                           (cdr inner-second)
                           color)
             (add-triangle list
                           (car outer-first)
                           (cdr outer-first)
                           (car inner-second)
                           (cdr inner-second)
                           (car inner-first)
                           (cdr inner-first)
                           color))))

(defun add-gradient-rectangle (list
                               x
                               y
                               width
                               height
                               top-color
                               bottom-color)
  "Append a rectangle that shades from one color at the top to another at the bottom.

LIST: a draw list.
X: left edge.
Y: top edge.
WIDTH: width.
HEIGHT: height.
TOP-COLOR: an RGBA at the top edge.
BOTTOM-COLOR: an RGBA at the bottom edge."
  (declare (type single-float x y width height))
  (ensure-vertex-room list 6)
  (let ((first (draw-list-vertex-count list))
        (right (+ x width))
        (bottom (+ y height)))
    (write-vertex list
                  x
                  y
                  top-color)
    (write-vertex list
                  right
                  y
                  top-color)
    (write-vertex list
                  right
                  bottom
                  bottom-color)
    (write-vertex list
                  x
                  y
                  top-color)
    (write-vertex list
                  right
                  bottom
                  bottom-color)
    (write-vertex list
                  x
                  bottom
                  bottom-color)
    (note-primitives list
                     :triangles
                     first
                     6)))

(defun add-shadow (list
                   x
                   y
                   width
                   height
                   radius
                   spread
                   color)
  "Append a soft drop shadow under a rounded rectangle: translucent copies that grow and fade
outward, lowered a little so that the light seems to come from above. Draw it before the
rectangle itself.

LIST: a draw list.
X: left edge of the rectangle that casts it.
Y: top edge.
WIDTH: width.
HEIGHT: height.
RADIUS: corner radius of that rectangle.
SPREAD: how far the shadow reaches beyond it, in pixels.
COLOR: an RGBA; its opacity is the opacity of the whole shadow near the rectangle."
  (declare (type single-float x y width height radius spread))
  (dotimes (layer +shadow-layers+)
    (let* ((fraction (/ (- +shadow-layers+ layer) (float +shadow-layers+ 1f0)))
           (grow (* spread (/ (1+ layer) (float +shadow-layers+ 1f0)))))
      (add-rounded-rectangle list
                             (- x grow)
                             (+ (- y grow) (* 0.35f0 spread))
                             (+ width (* 2 grow))
                             (+ height (* 2 grow))
                             (+ radius grow)
                             (with-alpha color (/ (* fraction fraction) +shadow-layers+))))))

(defun add-dashed-line (list
                        x1
                        y1
                        x2
                        y2
                        dash
                        gap
                        color)
  "Append a dashed one-pixel line.

LIST: a draw list.
X1: start x in pixels.
Y1: start y in pixels.
X2: end x in pixels.
Y2: end y in pixels.
DASH: length of each dash in pixels.
GAP: length of each gap in pixels.
COLOR: an RGBA."
  (declare (type single-float x1 y1 x2 y2 dash gap))
  (let* ((dx (- x2 x1))
         (dy (- y2 y1))
         (length (sqrt (+ (* dx dx) (* dy dy))))
         (period (+ dash gap)))
    (when (and (> length 1f-6) (> period 1f-6))
      (loop for start from 0f0 below length by period
            do (let ((end (min length (+ start dash))))
                 (add-line list
                           (+ x1 (* dx (/ start length)))
                           (+ y1 (* dy (/ start length)))
                           (+ x1 (* dx (/ end length)))
                           (+ y1 (* dy (/ end length)))
                           color))))))

(defun add-polygon (list
                    points
                    color)
  "Append a filled convex polygon as a fan from its first point.

LIST: a draw list.
POINTS: a list of (x . y) conses, three or more, in order around the polygon.
COLOR: an RGBA."
  (loop for (second third) on (rest points)
        while third
        do (add-triangle list
                         (car (first points))
                         (cdr (first points))
                         (car second)
                         (cdr second)
                         (car third)
                         (cdr third)
                         color)))
