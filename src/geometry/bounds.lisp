(in-package #:borba-cad/geometry)

(defstruct (bounds (:constructor make-bounds (min-x
                                              min-y
                                              max-x
                                              max-y)))
  "An axis-aligned rectangle in world coordinates (millimetres).

MIN-X: smallest x.
MIN-Y: smallest y.
MAX-X: largest x; at least MIN-X.
MAX-Y: largest y; at least MIN-Y."
  (min-x 0d0 :type double-float :read-only t)
  (min-y 0d0 :type double-float :read-only t)
  (max-x 0d0 :type double-float :read-only t)
  (max-y 0d0 :type double-float :read-only t))

(defun bounds-from-corners (x1
                            y1
                            x2
                            y2)
  "Return the bounds of two opposite corners given in any order.

X1: x of the first corner.
Y1: y of the first corner.
X2: x of the opposite corner.
Y2: y of the opposite corner."
  (declare (type double-float x1 y1 x2 y2))
  (make-bounds (min x1 x2)
               (min y1 y2)
               (max x1 x2)
               (max y1 y2)))

(declaim (inline bounds-width bounds-height bounds-center-x bounds-center-y))
(defun bounds-width (bounds)
  "Return the extent of BOUNDS along x.

BOUNDS: a bounds value."
  (- (bounds-max-x bounds) (bounds-min-x bounds)))

(defun bounds-height (bounds)
  "Return the extent of BOUNDS along y.

BOUNDS: a bounds value."
  (- (bounds-max-y bounds) (bounds-min-y bounds)))

(defun bounds-center-x (bounds)
  "Return the x of the center of BOUNDS.

BOUNDS: a bounds value."
  (* 0.5d0 (+ (bounds-min-x bounds) (bounds-max-x bounds))))

(defun bounds-center-y (bounds)
  "Return the y of the center of BOUNDS.

BOUNDS: a bounds value."
  (* 0.5d0 (+ (bounds-min-y bounds) (bounds-max-y bounds))))

(defun bounds-union (first
                     second)
  "Return the smallest bounds containing both arguments.

FIRST: a bounds value.
SECOND: a bounds value."
  (make-bounds (min (bounds-min-x first) (bounds-min-x second))
               (min (bounds-min-y first) (bounds-min-y second))
               (max (bounds-max-x first) (bounds-max-x second))
               (max (bounds-max-y first) (bounds-max-y second))))

(declaim (inline bounds-intersect-p bounds-contains-bounds-p bounds-contains-point-p))
(defun bounds-intersect-p (first
                           second)
  "Return true when the two bounds share at least one point (touching counts).

FIRST: a bounds value.
SECOND: a bounds value."
  (and (<= (bounds-min-x first) (bounds-max-x second))
       (<= (bounds-min-x second) (bounds-max-x first))
       (<= (bounds-min-y first) (bounds-max-y second))
       (<= (bounds-min-y second) (bounds-max-y first))))

(defun bounds-contains-bounds-p (outer
                                 inner)
  "Return true when INNER lies entirely inside OUTER (touching edges counts).

OUTER: the containing bounds.
INNER: the bounds that must fit."
  (and (<= (bounds-min-x outer) (bounds-min-x inner))
       (<= (bounds-min-y outer) (bounds-min-y inner))
       (>= (bounds-max-x outer) (bounds-max-x inner))
       (>= (bounds-max-y outer) (bounds-max-y inner))))

(defun bounds-contains-point-p (bounds
                                x
                                y)
  "Return true when the point lies inside BOUNDS (the boundary counts).

BOUNDS: a bounds value.
X: point x.
Y: point y."
  (declare (type double-float x y))
  (and (<= (bounds-min-x bounds) x (bounds-max-x bounds))
       (<= (bounds-min-y bounds) y (bounds-max-y bounds))))

(defun expand-bounds (bounds
                      margin)
  "Return BOUNDS grown by MARGIN on every side (shrunk when MARGIN is negative).

BOUNDS: a bounds value.
MARGIN: distance added on each side, in millimetres."
  (declare (type double-float margin))
  (make-bounds (- (bounds-min-x bounds) margin)
               (- (bounds-min-y bounds) margin)
               (+ (bounds-max-x bounds) margin)
               (+ (bounds-max-y bounds) margin)))
