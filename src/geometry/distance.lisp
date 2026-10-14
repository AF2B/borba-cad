(in-package #:borba-cad/geometry)

(defconstant +two-pi+ (* 2 (float pi 1d0))
  "A full turn in radians.")

(declaim (inline distance))
(defun distance (x1
                 y1
                 x2
                 y2)
  "Return the Euclidean distance between two points.

X1: x of the first point.
Y1: y of the first point.
X2: x of the second point.
Y2: y of the second point."
  (declare (type double-float x1 y1 x2 y2))
  (let ((dx (- x2 x1))
        (dy (- y2 y1)))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun normalize-angle (radians)
  "Return RADIANS reduced to the range [0, 2 pi).

RADIANS: any finite angle in radians."
  (declare (type double-float radians))
  (let ((reduced (mod radians +two-pi+)))
    ;; MOD can return exactly 2 pi for tiny negative inputs after rounding.
    (if (>= reduced +two-pi+) 0d0 reduced)))

(declaim (inline segment-length))
(defun segment-length (x1
                       y1
                       x2
                       y2)
  "Return the length of the segment between two points.

X1: x of the start.
Y1: y of the start.
X2: x of the end.
Y2: y of the end."
  (distance x1
            y1
            x2
            y2))

(defun segment-angle (x1
                      y1
                      x2
                      y2)
  "Return the direction of the segment in [0, 2 pi), counter-clockwise from +x.

X1: x of the start.
Y1: y of the start.
X2: x of the end.
Y2: y of the end."
  (declare (type double-float x1 y1 x2 y2))
  (normalize-angle (atan (- y2 y1) (- x2 x1))))

(defun snap-coordinate (value
                        spacing)
  "Return the multiple of SPACING nearest to VALUE (ties go toward +infinity).

VALUE: the coordinate to snap.
SPACING: grid spacing, a positive length."
  (declare (type double-float value spacing))
  (* spacing (ffloor (+ (/ value spacing) 0.5d0))))

(declaim (inline closest-point-on-segment))
(defun closest-point-on-segment (px
                                 py
                                 x1
                                 y1
                                 x2
                                 y2)
  "Return the point of the segment nearest to P as three values: x, y and the parameter t in
[0, 1] along the segment. A zero-length segment yields its single point with t = 0.

PX: point x.
PY: point y.
X1: x of the segment start.
Y1: y of the segment start.
X2: x of the segment end.
Y2: y of the segment end."
  (declare (type double-float px py x1 y1 x2 y2))
  (let* ((dx (- x2 x1))
         (dy (- y2 y1))
         (length-squared (+ (* dx dx) (* dy dy))))
    (if (<= length-squared 0d0)
        (values x1 y1 0d0)
        (let ((parameter (max 0d0 (min 1d0 (/ (+ (* (- px x1) dx) (* (- py y1) dy))
                                              length-squared)))))
          (declare (type double-float parameter))
          (values (+ x1 (* parameter dx))
                  (+ y1 (* parameter dy))
                  parameter)))))

(declaim (inline point-segment-distance))
(defun point-segment-distance (px
                               py
                               x1
                               y1
                               x2
                               y2)
  "Return the distance from point P to the segment (not the infinite line).

PX: point x.
PY: point y.
X1: x of the segment start.
Y1: y of the segment start.
X2: x of the segment end.
Y2: y of the segment end."
  (declare (type double-float px py x1 y1 x2 y2))
  (multiple-value-bind (closest-x closest-y)
      (closest-point-on-segment px
                                py
                                x1
                                y1
                                x2
                                y2)
    (declare (type double-float closest-x closest-y))
    (distance px
              py
              closest-x
              closest-y)))

(defun point-circle-outline-distance (px
                                      py
                                      center-x
                                      center-y
                                      radius)
  "Return the distance from point P to the circle outline (zero on the circle itself, and
equal to the radius at the center).

PX: point x.
PY: point y.
CENTER-X: circle center x.
CENTER-Y: circle center y.
RADIUS: circle radius."
  (declare (type double-float px py center-x center-y radius))
  (abs (- (distance px
                    py
                    center-x
                    center-y) radius)))

(defun point-rectangle-outline-distance (px
                                         py
                                         min-x
                                         min-y
                                         max-x
                                         max-y)
  "Return the distance from point P to the rectangle boundary, from inside or outside.

PX: point x.
PY: point y.
MIN-X: rectangle left edge.
MIN-Y: rectangle bottom edge.
MAX-X: rectangle right edge.
MAX-Y: rectangle top edge."
  (declare (type double-float px py min-x min-y max-x max-y))
  (let ((overshoot-x (max (- min-x px) (- px max-x)))
        (overshoot-y (max (- min-y py) (- py max-y))))
    (if (or (> overshoot-x 0d0) (> overshoot-y 0d0))
        (sqrt (+ (expt (max overshoot-x 0d0) 2) (expt (max overshoot-y 0d0) 2)))
        (- (max overshoot-x overshoot-y)))))

(defun point-polyline-distance (px
                                py
                                coordinates
                                closed)
  "Return the distance from point P to a polyline given as flat x/y pairs.

PX: point x.
PY: point y.
COORDINATES: simple-array of double-float, x0 y0 x1 y1 ..., at least two vertices.
CLOSED: when true, the segment from the last vertex back to the first is included."
  (declare (type double-float px py)
           (type (simple-array double-float (*)) coordinates))
  (let* ((vertex-count (floor (length coordinates) 2))
         (best most-positive-double-float))
    (declare (type double-float best) (type fixnum vertex-count))
    (loop for index from 0 below (1- vertex-count)
          do (setf best (min best
                             (point-segment-distance px
                                                     py
                                                     (aref coordinates (* 2 index))
                                                     (aref coordinates (+ (* 2 index) 1))
                                                     (aref coordinates (+ (* 2 index) 2))
                                                     (aref coordinates (+ (* 2 index) 3))))))
    (when (and closed (> vertex-count 2))
      (setf best (min best
                      (point-segment-distance px
                                              py
                                              (aref coordinates (- (length coordinates) 2))
                                              (aref coordinates (- (length coordinates) 1))
                                              (aref coordinates 0)
                                              (aref coordinates 1)))))
    best))

(declaim (inline bounds-near-point-p))
(defun bounds-near-point-p (bounds
                            x
                            y
                            radius)
  "Return true when the point lies within RADIUS of BOUNDS along both axes (a cheap
pre-filter: every shape inside BOUNDS closer than RADIUS to the point passes it).

BOUNDS: a bounds value.
X: point x.
Y: point y.
RADIUS: non-negative search radius."
  (declare (type double-float x y radius))
  (and (>= x (- (bounds-min-x bounds) radius))
       (<= x (+ (bounds-max-x bounds) radius))
       (>= y (- (bounds-min-y bounds) radius))
       (<= y (+ (bounds-max-y bounds) radius))))

(defun point-bounds-distance (x
                              y
                              bounds)
  "Return the distance from a point to the filled rectangle BOUNDS (zero inside or on it).

X: point x.
Y: point y.
BOUNDS: a bounds value."
  (declare (type double-float x y))
  (let ((overshoot-x (max (- (bounds-min-x bounds) x) 0d0 (- x (bounds-max-x bounds))))
        (overshoot-y (max (- (bounds-min-y bounds) y) 0d0 (- y (bounds-max-y bounds)))))
    (sqrt (+ (* overshoot-x overshoot-x) (* overshoot-y overshoot-y)))))

(defun point-oriented-box-distance (px
                                    py
                                    origin-x
                                    origin-y
                                    length
                                    ascent
                                    descent
                                    angle)
  "Return the distance from a point to a filled rotated box (zero inside or on it).
The box is the one described by ORIENTED-BOX-BOUNDS.

PX: point x.
PY: point y.
ORIGIN-X: x of the box origin.
ORIGIN-Y: y of the box origin.
LENGTH: extent along the direction.
ASCENT: extent on the left of the direction.
DESCENT: extent on the right of the direction.
ANGLE: direction in radians, counter-clockwise from +x."
  (declare (type double-float px py origin-x origin-y length ascent descent angle))
  (let* ((cosine (cos angle))
         (sine (sin angle))
         (dx (- px origin-x))
         (dy (- py origin-y))
         (along (+ (* dx cosine) (* dy sine)))
         (across (+ (* (- dx) sine) (* dy cosine))))
    (point-bounds-distance along
                           across
                           (make-bounds 0d0
                                        (- descent)
                                        length
                                        ascent))))
