(in-package #:borba-cad/geometry)

;;;; Measures that are not properties of one entity: the area a contour encloses and the angle between two rays.

(defun contour-area (coordinates)
  "Return the area enclosed by a closed contour whose vertices are given in order, by the shoelace formula. The
contour closes itself, so the first vertex is not repeated. A contour that crosses itself gives the difference
of its loops, which is what the formula means; fewer than three vertices enclose nothing.

COORDINATES: simple-array of double-float holding x0 y0 x1 y1 ..."
  (declare (type (simple-array double-float (*)) coordinates))
  (let ((count (polyline-vertex-count coordinates))
        (twice-area 0d0))
    (declare (type double-float twice-area))
    (when (>= count 3)
      (dotimes (index count)
        (let* ((next (mod (1+ index) count))
               (x1 (aref coordinates (* 2 index)))
               (y1 (aref coordinates (1+ (* 2 index))))
               (x2 (aref coordinates (* 2 next)))
               (y2 (aref coordinates (1+ (* 2 next)))))
          (incf twice-area (- (* x1 y2) (* x2 y1))))))
    (/ (abs twice-area) 2d0)))

(defun angle-between-rays (vertex-x
                           vertex-y
                           first-x
                           first-y
                           second-x
                           second-y)
  "Return the angle at a vertex between the rays toward two points, in radians from 0 to pi, or NIL when one of
the points is at the vertex and so gives no direction.

VERTEX-X: x of the vertex.
VERTEX-Y: y of the vertex.
FIRST-X: x of a point on the first ray.
FIRST-Y: y of a point on the first ray.
SECOND-X: x of a point on the second ray.
SECOND-Y: y of a point on the second ray."
  (declare (type double-float vertex-x vertex-y first-x first-y second-x second-y))
  (let ((ax (- first-x vertex-x))
        (ay (- first-y vertex-y))
        (bx (- second-x vertex-x))
        (by (- second-y vertex-y)))
    (and (> (distance 0d0
                      0d0
                      ax
                      ay) +length-tolerance+)
         (> (distance 0d0
                      0d0
                      bx
                      by) +length-tolerance+)
         (atan (abs (- (* ax by) (* ay bx)))
               (+ (* ax bx) (* ay by))))))
