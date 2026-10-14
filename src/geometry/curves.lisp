(in-package #:borba-cad/geometry)

;;;; Arcs, ellipses and regular polygons: bounds, distance from a point, measures, and whether they
;;;; touch a window. An arc is a part of a circle swept counter-clockwise from a start angle; an
;;;; ellipse is given by its two semi-axes and the direction of the first; a regular polygon by the
;;;; circle through its vertices, its number of sides and the direction of its first vertex.

(defconstant +half-pi+ (/ (float pi 1d0) 2)
  "A quarter of a turn in radians.")

(defconstant +ellipse-outline-points+ 128
  "Number of vertices of the polygon that stands for an ellipse where a polygon is wanted, as in the
test of whether an ellipse touches a window.")

(defconstant +ellipse-root-iterations+ 1100
  "Most halvings the search for the nearest point of an ellipse makes: enough to exhaust the
precision of a double.")

;;; Arcs

(defun arc-sweep (start-angle
                  end-angle)
  "Return how far the arc from START-ANGLE counter-clockwise to END-ANGLE turns, in [0, 2 pi).

START-ANGLE: the angle at which the arc starts, in radians.
END-ANGLE: the angle at which it ends, in radians."
  (declare (type double-float start-angle end-angle))
  (normalize-angle (- end-angle start-angle)))

(defun angle-within-sweep-p (angle
                             start-angle
                             sweep)
  "Return true when ANGLE lies on the arc that starts at START-ANGLE and turns SWEEP counter-clockwise,
its ends included, to the angle tolerance.

ANGLE: a direction in radians.
START-ANGLE: the angle at which the arc starts.
SWEEP: how far it turns, in (0, 2 pi)."
  (declare (type double-float angle start-angle sweep))
  (let ((from-start (normalize-angle (- angle start-angle))))
    (or (<= from-start (+ sweep +angle-tolerance+))
        (>= from-start (- +two-pi+ +angle-tolerance+)))))

(defun arc-end-point (center-x
                      center-y
                      radius
                      angle)
  "Return, as two values, the point of the circle at ANGLE.

CENTER-X: x of the center.
CENTER-Y: y of the center.
RADIUS: the radius.
ANGLE: direction from the center, in radians."
  (declare (type double-float center-x center-y radius angle))
  (values (+ center-x (* radius (cos angle)))
          (+ center-y (* radius (sin angle)))))

(defun arc-bounds (center-x
                   center-y
                   radius
                   start-angle
                   sweep)
  "Return the bounds of an arc: its two ends and the points of the circle farthest along an axis
that the arc passes through.

CENTER-X: x of the center.
CENTER-Y: y of the center.
RADIUS: the radius.
START-ANGLE: the angle at which the arc starts.
SWEEP: how far it turns counter-clockwise, in (0, 2 pi)."
  (declare (type double-float center-x center-y radius start-angle sweep))
  (let ((min-x most-positive-double-float)
        (min-y most-positive-double-float)
        (max-x most-negative-double-float)
        (max-y most-negative-double-float))
    (flet ((include (angle)
             (multiple-value-bind (x y)
                 (arc-end-point center-x
                                center-y
                                radius
                                angle)
               (setf min-x (min min-x x)
                     min-y (min min-y y)
                     max-x (max max-x x)
                     max-y (max max-y y)))))
      (include start-angle)
      (include (+ start-angle sweep))
      (dotimes (quarter 4)
        (let ((angle (* quarter +half-pi+)))
          (when (angle-within-sweep-p angle
                                      start-angle
                                      sweep)
            (include angle)))))
    (make-bounds min-x
                 min-y
                 max-x
                 max-y)))

(defun arc-length (radius
                   sweep)
  "Return the length of an arc.

RADIUS: the radius.
SWEEP: how far it turns, in radians."
  (declare (type double-float radius sweep))
  (* radius sweep))

(defun point-arc-distance (px
                           py
                           center-x
                           center-y
                           radius
                           start-angle
                           sweep)
  "Return the distance from a point to an arc: to the circle where the point lies in the direction of
the arc, and to the nearer end otherwise. From the center it is the radius.

PX: point x.
PY: point y.
CENTER-X: x of the center.
CENTER-Y: y of the center.
RADIUS: the radius.
START-ANGLE: the angle at which the arc starts.
SWEEP: how far it turns counter-clockwise."
  (declare (type double-float px py center-x center-y radius start-angle sweep))
  (let ((dx (- px center-x))
        (dy (- py center-y)))
    (cond ((and (zerop dx) (zerop dy))
           radius)
          ((angle-within-sweep-p (atan dy dx)
                                 start-angle
                                 sweep)
           (abs (- (sqrt (+ (* dx dx) (* dy dy))) radius)))
          (t
           (multiple-value-bind (x1 y1)
               (arc-end-point center-x
                              center-y
                              radius
                              start-angle)
             (multiple-value-bind (x2 y2)
                 (arc-end-point center-x
                                center-y
                                radius
                                (+ start-angle sweep))
               (min (distance px
                              py
                              x1
                              y1)
                    (distance px
                              py
                              x2
                              y2))))))))

(defun circle-segment-intersection-angles (center-x
                                           center-y
                                           radius
                                           x1
                                           y1
                                           x2
                                           y2)
  "Return the directions, from the center, of the points where a circle crosses a segment: none, one
or two.

CENTER-X: x of the center.
CENTER-Y: y of the center.
RADIUS: the radius.
X1: x of the segment start.
Y1: y of the segment start.
X2: x of the segment end.
Y2: y of the segment end."
  (declare (type double-float center-x center-y radius x1 y1 x2 y2))
  (let* ((dx (- x2 x1))
         (dy (- y2 y1))
         (fx (- x1 center-x))
         (fy (- y1 center-y))
         (a (+ (* dx dx) (* dy dy)))
         (b (* 2 (+ (* fx dx) (* fy dy))))
         (c (- (+ (* fx fx) (* fy fy)) (* radius radius)))
         (discriminant (- (* b b) (* 4 a c)))
         (angles '()))
    (when (and (> a 0d0) (>= discriminant 0d0))
      (let ((root (sqrt discriminant)))
        (dolist (parameter (list (/ (- (- b) root) (* 2 a))
                                 (/ (+ (- b) root) (* 2 a))))
          (when (<= (- +length-tolerance+) parameter (+ 1d0 +length-tolerance+))
            (push (atan (+ fy (* parameter dy)) (+ fx (* parameter dx))) angles)))))
    angles))

(defun arc-touches-bounds-p (center-x
                             center-y
                             radius
                             start-angle
                             sweep
                             bounds)
  "Return true when an arc has a point inside BOUNDS or on its edge. The arc is one piece, so if
a part of it is in the window either an end is, or the arc crosses an edge of the window on its way.

CENTER-X: x of the center.
CENTER-Y: y of the center.
RADIUS: the radius.
START-ANGLE: the angle at which the arc starts.
SWEEP: how far it turns counter-clockwise.
BOUNDS: the window."
  (declare (type double-float center-x center-y radius start-angle sweep))
  (flet ((end-inside-p (angle)
           (multiple-value-bind (x y)
               (arc-end-point center-x
                              center-y
                              radius
                              angle)
             (bounds-contains-point-p bounds
                                      x
                                      y)))
         (edge-crossed-p (x1 y1 x2 y2)
           (some (lambda (angle) (angle-within-sweep-p angle
                                                       start-angle
                                                       sweep))
                 (circle-segment-intersection-angles center-x
                                                     center-y
                                                     radius
                                                     x1
                                                     y1
                                                     x2
                                                     y2))))
    (let ((left (bounds-min-x bounds))
          (bottom (bounds-min-y bounds))
          (right (bounds-max-x bounds))
          (top (bounds-max-y bounds)))
      (or (end-inside-p start-angle)
          (end-inside-p (+ start-angle sweep))
          (edge-crossed-p left bottom right bottom)
          (edge-crossed-p right bottom right top)
          (edge-crossed-p right top left top)
          (edge-crossed-p left top left bottom)))))

;;; Ellipses

(defun ellipse-half-extents (radius-x
                             radius-y
                             rotation)
  "Return, as two values, how far an ellipse reaches from its center along x and along y.

RADIUS-X: the semi-axis in the direction ROTATION.
RADIUS-Y: the semi-axis across it.
ROTATION: direction of the first axis, in radians."
  (declare (type double-float radius-x radius-y rotation))
  (let ((cosine (cos rotation))
        (sine (sin rotation)))
    (values (sqrt (+ (* radius-x radius-x cosine cosine) (* radius-y radius-y sine sine)))
            (sqrt (+ (* radius-x radius-x sine sine) (* radius-y radius-y cosine cosine))))))

(defun ellipse-bounds (center-x
                       center-y
                       radius-x
                       radius-y
                       rotation)
  "Return the bounds of an ellipse.

CENTER-X: x of the center.
CENTER-Y: y of the center.
RADIUS-X: the semi-axis in the direction ROTATION.
RADIUS-Y: the semi-axis across it.
ROTATION: direction of the first axis, in radians."
  (declare (type double-float center-x center-y radius-x radius-y rotation))
  (multiple-value-bind (half-width half-height)
      (ellipse-half-extents radius-x
                            radius-y
                            rotation)
    (make-bounds (- center-x half-width)
                 (- center-y half-height)
                 (+ center-x half-width)
                 (+ center-y half-height))))

(defun ellipse-area (radius-x
                     radius-y)
  "Return the area of an ellipse.

RADIUS-X: one semi-axis.
RADIUS-Y: the other."
  (declare (type double-float radius-x radius-y))
  (* (float pi 1d0) radius-x radius-y))

(defun ellipse-perimeter (radius-x
                          radius-y)
  "Return the perimeter of an ellipse, by Ramanujan's second approximation, which is within a
few parts per billion for the ellipses of a drawing.

RADIUS-X: one semi-axis.
RADIUS-Y: the other."
  (declare (type double-float radius-x radius-y))
  (let* ((sum (+ radius-x radius-y))
         (h (expt (/ (- radius-x radius-y) sum) 2)))
    (* (float pi 1d0) sum (+ 1d0 (/ (* 3 h) (+ 10d0 (sqrt (- 4d0 (* 3 h)))))))))

(defun ellipse-points (center-x
                       center-y
                       radius-x
                       radius-y
                       rotation
                       count)
  "Return the vertices of the polygon of COUNT sides inscribed in an ellipse, starting at the end
of the first axis, as a vector of alternating x and y values.

CENTER-X: x of the center.
CENTER-Y: y of the center.
RADIUS-X: the semi-axis in the direction ROTATION.
RADIUS-Y: the semi-axis across it.
ROTATION: direction of the first axis, in radians.
COUNT: number of vertices."
  (declare (type double-float center-x center-y radius-x radius-y rotation)
           (type (integer 3) count))
  (let ((points (make-array (* 2 count) :element-type 'double-float))
        (cosine (cos rotation))
        (sine (sin rotation)))
    (dotimes (index count)
      (let* ((parameter (* +two-pi+ (/ index (float count 1d0))))
             (u (* radius-x (cos parameter)))
             (v (* radius-y (sin parameter))))
        (setf (aref points (* 2 index)) (+ center-x (* u cosine) (- (* v sine)))
              (aref points (1+ (* 2 index))) (+ center-y (* u sine) (* v cosine)))))
    points))

(defun ellipse-touches-bounds-p (center-x
                                 center-y
                                 radius-x
                                 radius-y
                                 rotation
                                 bounds)
  "Return true when the outline of an ellipse has a point inside BOUNDS or on its edge, judged on
the polygon of +ELLIPSE-OUTLINE-POINTS+ sides inscribed in it.

CENTER-X: x of the center.
CENTER-Y: y of the center.
RADIUS-X: the semi-axis in the direction ROTATION.
RADIUS-Y: the semi-axis across it.
ROTATION: direction of the first axis, in radians.
BOUNDS: the window."
  (polyline-touches-bounds-p (ellipse-points center-x
                                             center-y
                                             radius-x
                                             radius-y
                                             rotation
                                             +ellipse-outline-points+)
                             t
                             bounds))

(defun nearest-ellipse-root (ratio
                             z0
                             z1
                             g)
  "Find the root of the equation that locates the nearest point of an ellipse, by bisection (the
method of D. Eberly, Distance from a Point to an Ellipse, Geometric Tools).

RATIO: the square of the ratio of the semi-axes, the larger over the smaller.
Z0: the first coordinate of the point over the larger semi-axis.
Z1: the second coordinate of the point over the smaller semi-axis.
G: the value of the ellipse equation at the point."
  (declare (type double-float ratio z0 z1 g))
  (let* ((n0 (* ratio z0))
         (low (- z1 1d0))
         (high (if (< g 0d0) 0d0 (- (sqrt (+ (* n0 n0) (* z1 z1))) 1d0)))
         (root 0d0))
    (declare (type double-float n0 low high root))
    (dotimes (iteration +ellipse-root-iterations+)
      (setf root (/ (+ low high) 2))
      (when (or (= root low) (= root high))
        (return))
      (let* ((ratio0 (/ n0 (+ root ratio)))
             (ratio1 (/ z1 (+ root 1d0)))
             (value (- (+ (* ratio0 ratio0) (* ratio1 ratio1)) 1d0)))
        (cond ((> value 0d0) (setf low root))
              ((< value 0d0) (setf high root))
              (t (return)))))
    root))

(defun point-ellipse-distance (px
                               py
                               center-x
                               center-y
                               radius-x
                               radius-y
                               rotation)
  "Return the distance from a point to the outline of an ellipse (zero on it).

PX: point x.
PY: point y.
CENTER-X: x of the center.
CENTER-Y: y of the center.
RADIUS-X: the semi-axis in the direction ROTATION.
RADIUS-Y: the semi-axis across it.
ROTATION: direction of the first axis, in radians."
  (declare (type double-float px py center-x center-y radius-x radius-y rotation))
  (let* ((dx (- px center-x))
         (dy (- py center-y))
         (cosine (cos rotation))
         (sine (sin rotation))
         (u (abs (+ (* dx cosine) (* dy sine))))
         (v (abs (+ (* (- dx) sine) (* dy cosine))))
         (swap (< radius-x radius-y))
         (e0 (if swap radius-y radius-x))
         (e1 (if swap radius-x radius-y))
         (y0 (if swap v u))
         (y1 (if swap u v)))
    (declare (type double-float dx dy cosine sine u v e0 e1 y0 y1))
    ;; The point is brought to the first quadrant of an ellipse whose first axis is the longer.
    (cond ((> y1 0d0)
           (cond ((> y0 0d0)
                  (let* ((z0 (/ y0 e0))
                         (z1 (/ y1 e1))
                         (g (- (+ (* z0 z0) (* z1 z1)) 1d0)))
                    (if (zerop g)
                        0d0
                        (let* ((ratio (expt (/ e0 e1) 2))
                               (root (nearest-ellipse-root ratio
                                                           z0
                                                           z1
                                                           g))
                               (x0 (/ (* ratio y0) (+ root ratio)))
                               (x1 (/ y1 (+ root 1d0))))
                          (distance x0
                                    x1
                                    y0
                                    y1)))))
                 (t (abs (- y1 e1)))))
          (t
           (let ((numerator (* e0 y0))
                 (denominator (- (* e0 e0) (* e1 e1))))
             (if (< numerator denominator)
                 (let* ((x0-over-e0 (/ numerator denominator))
                        (x0 (* e0 x0-over-e0))
                        (x1 (* e1 (sqrt (- 1d0 (* x0-over-e0 x0-over-e0))))))
                   (sqrt (+ (expt (- x0 y0) 2) (* x1 x1))))
                 (abs (- y0 e0))))))))

;;; Regular polygons

(defun regular-polygon-vertices (center-x
                                 center-y
                                 radius
                                 sides
                                 rotation)
  "Return the vertices of a regular polygon, the first at the angle ROTATION, as a vector of
alternating x and y values.

CENTER-X: x of the center.
CENTER-Y: y of the center.
RADIUS: the radius of the circle through the vertices.
SIDES: number of sides, at least 3.
ROTATION: direction of the first vertex, in radians."
  (declare (type double-float center-x center-y radius rotation)
           (type (integer 3) sides))
  (let ((points (make-array (* 2 sides) :element-type 'double-float)))
    (dotimes (index sides)
      (let ((angle (+ rotation (* +two-pi+ (/ index (float sides 1d0))))))
        (setf (aref points (* 2 index)) (+ center-x (* radius (cos angle)))
              (aref points (1+ (* 2 index))) (+ center-y (* radius (sin angle))))))
    points))

(defun regular-polygon-side-length (radius
                                    sides)
  "Return the length of a side of a regular polygon.

RADIUS: the radius of the circle through the vertices.
SIDES: number of sides."
  (declare (type double-float radius))
  (* 2 radius (sin (/ (float pi 1d0) sides))))

(defun regular-polygon-area (radius
                             sides)
  "Return the area of a regular polygon.

RADIUS: the radius of the circle through the vertices.
SIDES: number of sides."
  (declare (type double-float radius))
  (* 0.5d0 sides radius radius (sin (/ +two-pi+ sides))))
