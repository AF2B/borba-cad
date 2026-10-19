(in-package #:borba-cad/geometry)

;;;; What fills a region: whether a point is inside a polygon, the parallel lines that hatch it, and the triangles that
;;;; make it solid. A polygon is a flat array x0 y0 x1 y1 ... of its vertices in order; the last joins the first, and
;;;; the polygon may be concave but is taken to be simple.

(defconstant +ear-tolerance+ 1d-9
  "How far from straight, as a fraction of the product of the two sides, a corner of a polygon must turn for it to
count as a corner when the polygon is cut into triangles.")

(defun contour-signed-area (coordinates)
  "Return the area of a polygon, positive when it goes around counter-clockwise and negative when clockwise.

COORDINATES: simple-array of double-float holding x0 y0 x1 y1 ..."
  (declare (type (simple-array double-float (*)) coordinates))
  (let ((count (polyline-vertex-count coordinates))
        (twice-area 0d0))
    (declare (type double-float twice-area))
    (dotimes (index count)
      (let ((next (mod (1+ index) count)))
        (incf twice-area (- (* (aref coordinates (* 2 index)) (aref coordinates (1+ (* 2 next))))
                            (* (aref coordinates (* 2 next)) (aref coordinates (1+ (* 2 index))))))))
    (/ twice-area 2d0)))

(defun point-in-polygon-p (x
                           y
                           coordinates)
  "Return true when a point is inside a polygon, by the even-odd rule. A point on the boundary may be either.

X: x of the point.
Y: y of the point.
COORDINATES: simple-array of double-float holding x0 y0 x1 y1 ..."
  (declare (type double-float x y)
           (type (simple-array double-float (*)) coordinates))
  (let ((count (polyline-vertex-count coordinates))
        (inside nil))
    (dotimes (index count)
      (let* ((next (mod (1+ index) count))
             (x1 (aref coordinates (* 2 index)))
             (y1 (aref coordinates (1+ (* 2 index))))
             (x2 (aref coordinates (* 2 next)))
             (y2 (aref coordinates (1+ (* 2 next)))))
        (when (and (not (eq (> y1 y) (> y2 y)))
                   (< x (+ x1 (* (- x2 x1) (/ (- y y1) (- y2 y1))))))
          (setf inside (not inside)))))
    inside))

(defun hatch-line-count (coordinates
                         angle
                         spacing
                         origin-x
                         origin-y)
  "Return how many parallel lines, SPACING apart and in the direction ANGLE, cross a polygon.

COORDINATES: simple-array of double-float holding x0 y0 x1 y1 ...
ANGLE: direction of the lines, in radians.
SPACING: distance between two lines.
ORIGIN-X: x of the point one of the lines passes through.
ORIGIN-Y: y of that point."
  (declare (type (simple-array double-float (*)) coordinates)
           (type double-float angle spacing origin-x origin-y))
  (let ((normal-x (- (sin angle)))
        (normal-y (cos angle))
        (low nil)
        (high nil))
    (loop for index from 0 below (length coordinates) by 2
          do (let ((value (+ (* (- (aref coordinates index) origin-x) normal-x)
                             (* (- (aref coordinates (1+ index)) origin-y) normal-y))))
               (setf low (if low (min low value) value)
                     high (if high (max high value) value))))
    (if low
        (1+ (floor (- high low) spacing))
        0)))

(defun hatch-segments (coordinates
                       angle
                       spacing
                       origin-x
                       origin-y
                       &optional holes)
  "Return the segments of the parallel lines that hatch a polygon, as a simple array of double-float holding x1 y1 x2 y2
for each. The lines are SPACING apart in the direction ANGLE, one of them through the origin, and are cut where they
leave the polygon, so a concave polygon is hatched only inside, and where they enter a hole.

COORDINATES: simple-array of double-float holding x0 y0 x1 y1 ...
ANGLE: direction of the lines, in radians.
SPACING: distance between two lines, above zero.
ORIGIN-X: x of the point one of the lines passes through.
ORIGIN-Y: y of that point.
HOLES: a list of polygons, each such an array, that are left unhatched."
  (declare (type (simple-array double-float (*)) coordinates)
           (type double-float angle spacing origin-x origin-y))
  (let* ((direction-x (cos angle))
         (direction-y (sin angle))
         (normal-x (- direction-y))
         (normal-y direction-x)
         (rings (loop for ring in (cons coordinates holes)
                      collect (let* ((count (polyline-vertex-count ring))
                                     (along (make-array count :element-type 'double-float))
                                     (across (make-array count :element-type 'double-float)))
                                (dotimes (index count)
                                  (let ((dx (- (aref ring (* 2 index)) origin-x))
                                        (dy (- (aref ring (1+ (* 2 index))) origin-y)))
                                    (setf (aref along index) (+ (* dx direction-x) (* dy direction-y))
                                          (aref across index) (+ (* dx normal-x) (* dy normal-y)))))
                                (list count along across))))
         (result '()))
    (let ((low (reduce #'min (third (first rings))))
          (high (reduce #'max (third (first rings)))))
      (loop for line from (ceiling low spacing) to (floor high spacing)
            for level = (* line spacing)
            do (when (< (+ low +length-tolerance+) level (- high +length-tolerance+))
                 (let ((crossings '()))
                   (dolist (ring rings)
                     (destructuring-bind (count along across) ring
                       (dotimes (index count)
                         (let* ((next (mod (1+ index) count))
                                (level-1 (aref across index))
                                (level-2 (aref across next)))
                           (when (not (eq (> level-1 level) (> level-2 level)))
                             (push (+ (aref along index)
                                      (* (- (aref along next) (aref along index))
                                         (/ (- level level-1) (- level-2 level-1))))
                                   crossings))))))
                   (loop for (start end) on (sort crossings #'<) by #'cddr
                         while end
                         do (when (> (- end start) +length-tolerance+)
                              (push (list (+ origin-x (* start direction-x) (* level normal-x))
                                          (+ origin-y (* start direction-y) (* level normal-y))
                                          (+ origin-x (* end direction-x) (* level normal-x))
                                          (+ origin-y (* end direction-y) (* level normal-y)))
                                    result)))))))
    (make-array (* 4 (length result))
                :element-type 'double-float
                :initial-contents (loop for segment in (nreverse result)
                                        append segment))))

(defun point-in-rings-p (x
                         y
                         coordinates
                         holes)
  "Return true when a point is inside a polygon and not inside any of its holes.

X: x of the point.
Y: y of the point.
COORDINATES: the polygon, a simple-array of double-float holding x0 y0 x1 y1 ...
HOLES: a list of polygons, each such an array."
  (and (point-in-polygon-p x
                           y
                           coordinates)
       (notany (lambda (hole) (point-in-polygon-p x
                                                  y
                                                  hole)) holes)))

(defun corner-turn (x1
                    y1
                    x2
                    y2
                    x3
                    y3)
  "Return twice the signed area of the triangle of three points: positive when they turn counter-clockwise.

X1: x of the first point.
Y1: y of the first point.
X2: x of the second point.
Y2: y of the second point.
X3: x of the third point.
Y3: y of the third point."
  (- (* (- x2 x1) (- y3 y1)) (* (- y2 y1) (- x3 x1))))

(defun point-in-triangle-p (x
                            y
                            x1
                            y1
                            x2
                            y2
                            x3
                            y3)
  "Return true when a point is inside, or on, a triangle given counter-clockwise.

X: x of the point.
Y: y of the point.
X1: x of the first corner.
Y1: y of the first corner.
X2: x of the second corner.
Y2: y of the second corner.
X3: x of the third corner.
Y3: y of the third corner."
  (and (>= (corner-turn x1
                        y1
                        x2
                        y2
                        x
                        y) 0d0)
       (>= (corner-turn x2
                        y2
                        x3
                        y3
                        x
                        y) 0d0)
       (>= (corner-turn x3
                        y3
                        x1
                        y1
                        x
                        y) 0d0)))

(defun triangulate-polygon (coordinates)
  "Return the triangles that cover a simple polygon, as a simple array of double-float holding the six coordinates of
each, by cutting off one ear after another. A corner that is straight is dropped without a triangle, and a polygon
that has no ear to cut, which only a polygon that crosses itself can be, is finished as a fan.

COORDINATES: simple-array of double-float holding x0 y0 x1 y1 ..."
  (declare (type (simple-array double-float (*)) coordinates))
  (let* ((count (polyline-vertex-count coordinates))
         (counter-clockwise (>= (contour-signed-area coordinates) 0d0))
         (before (make-array count))
         (after (make-array count))
         (remaining count)
         (current 0)
         (stalled 0)
         (triangles '()))
    (dotimes (index count)
      (setf (aref after index) (mod (if counter-clockwise (1+ index) (1- index)) count)
            (aref before index) (mod (if counter-clockwise (1- index) (1+ index)) count)))
    (flet ((x (index) (aref coordinates (* 2 index)))
           (y (index) (aref coordinates (1+ (* 2 index)))))
      (labels ((cut (vertex)
                 (let ((previous (aref before vertex))
                       (next (aref after vertex)))
                   (setf (aref after previous) next
                         (aref before next) previous)
                   (decf remaining)))
               (triangle (a
                          b
                          c)
                 (list (x a) (y a) (x b) (y b) (x c) (y c)))
               (ear-free-p (previous
                            vertex
                            next)
                 (loop for other = (aref after next) then (aref after other)
                       until (= other previous)
                       never (point-in-triangle-p (x other)
                                                  (y other)
                                                  (x previous)
                                                  (y previous)
                                                  (x vertex)
                                                  (y vertex)
                                                  (x next)
                                                  (y next)))))
        (loop while (and (> remaining 3) (<= stalled remaining))
              do (let* ((previous (aref before current))
                        (next (aref after current))
                        (turn (corner-turn (x previous)
                                           (y previous)
                                           (x current)
                                           (y current)
                                           (x next)
                                           (y next)))
                        (scale (* (distance (x previous)
                                            (y previous)
                                            (x current)
                                            (y current))
                                  (distance (x current)
                                            (y current)
                                            (x next)
                                            (y next)))))
                   (cond ((<= (abs turn) (* +ear-tolerance+ scale))
                          (cut current)
                          (setf current next
                                stalled 0))
                         ((and (> turn 0d0) (ear-free-p previous current next))
                          (push (triangle previous current next) triangles)
                          (cut current)
                          (setf current next
                                stalled 0))
                         (t (setf current next)
                            (incf stalled)))))
        (cond ((> remaining 3)
               (let ((first current))
                 (loop for second = (aref after first) then third
                       for third = (aref after second)
                       until (= third first)
                       do (push (triangle first second third) triangles))))
              ((= remaining 3)
               (let ((previous (aref before current))
                     (next (aref after current)))
                 (when (> (corner-turn (x previous)
                                       (y previous)
                                       (x current)
                                       (y current)
                                       (x next)
                                       (y next)) 0d0)
                   (push (triangle previous current next) triangles)))))))
    (make-array (* 6 (length triangles))
                :element-type 'double-float
                :initial-contents (loop for triangle in (nreverse triangles)
                                        append triangle))))

(defun region-triangles (coordinates
                         holes)
  "Return the triangles that cover a polygon without its holes, as a simple array of double-float holding the six
coordinates of each, by cutting the region into slabs at the height of every vertex and each slab, between the edges
that cross it, into trapezoids of two triangles. It does not matter how the holes lie in the polygon.

COORDINATES: the polygon, a simple-array of double-float holding x0 y0 x1 y1 ...
HOLES: a list of polygons, each such an array."
  (let* ((rings (cons coordinates holes))
         (heights (sort (remove-duplicates
                         (loop for ring in rings
                               append (loop for index from 1 below (length ring) by 2
                                            collect (aref ring index)))
                         :test (lambda (a b) (< (abs (- a b)) 1d-9)))
                        #'<))
         (triangles '()))
    (loop for (low high) on heights
          while high
          do (let ((edges '()))
               (dolist (ring rings)
                 (let ((count (polyline-vertex-count ring)))
                   (dotimes (index count)
                     (let* ((next (mod (1+ index) count))
                            (x1 (aref ring (* 2 index)))
                            (y1 (aref ring (1+ (* 2 index))))
                            (x2 (aref ring (* 2 next)))
                            (y2 (aref ring (1+ (* 2 next)))))
                       (when (and (/= y1 y2)
                                  (<= (min y1 y2) (+ low 1d-9))
                                  (>= (max y1 y2) (- high 1d-9)))
                         (push (list x1 y1 x2 y2) edges))))))
               (flet ((x-at (edge y)
                        (destructuring-bind (x1 y1 x2 y2) edge
                          (+ x1 (* (- x2 x1) (/ (- y y1) (- y2 y1)))))))
                 (let ((sorted (sort edges #'< :key (lambda (edge) (x-at edge (/ (+ low high) 2))))))
                   (loop for (left right) on sorted by #'cddr
                         while right
                         do (let ((bottom-left (x-at left low))
                                  (bottom-right (x-at right low))
                                  (top-left (x-at left high))
                                  (top-right (x-at right high)))
                              (when (> (+ (- bottom-right bottom-left) (- top-right top-left)) 1d-9)
                                (push (list bottom-left low bottom-right low top-right high) triangles)
                                (push (list bottom-left low top-right high top-left high) triangles))))))))
    (make-array (* 6 (length triangles))
                :element-type 'double-float
                :initial-contents (loop for triangle in (nreverse triangles)
                                        append triangle))))
