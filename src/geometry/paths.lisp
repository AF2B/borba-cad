(in-package #:borba-cad/geometry)

;;;; Walking along a path: a chain of segments, open or closed, given as a flat array of vertices x0 y0 x1 y1 ... Used
;;;; to put things at even steps along a line, a polyline or a curve.

(defconstant +path-distance-tolerance+ 1d-9
  "How far past a whole number of steps, relative to the step, a length may be and still count as that many steps.")

(defun path-segment-lengths (coordinates
                             closed)
  "Return the lengths of the segments of a path as a vector, in order; a closed path has one more, from the last vertex
back to the first.

COORDINATES: flat array of the vertices.
CLOSED: true when a segment joins the last vertex back to the first."
  (let* ((count (floor (length coordinates) 2))
         (segments (if closed count (max 0 (1- count))))
         (lengths (make-array segments :element-type 'double-float)))
    (dotimes (index segments lengths)
      (let ((next (mod (1+ index) count)))
        (setf (aref lengths index) (distance (aref coordinates (* 2 index))
                                             (aref coordinates (1+ (* 2 index)))
                                             (aref coordinates (* 2 next))
                                             (aref coordinates (1+ (* 2 next)))))))))

(defun path-length (coordinates
                    closed)
  "Return the length of a path.

COORDINATES: flat array of the vertices.
CLOSED: true when a segment joins the last vertex back to the first."
  (reduce #'+ (path-segment-lengths coordinates closed)))

(defun path-points-at (coordinates
                       closed
                       distances)
  "Return the points of a path at some distances from its start, as a list of (x y angle) lists, where the angle is the
direction of the path there, in radians. A vertex is given the direction of the segment that leaves it, and the end of
an open path that of its last segment. A distance past the end is taken to be the end.

COORDINATES: flat array of the vertices, with at least two apart.
CLOSED: true when a segment joins the last vertex back to the first.
DISTANCES: ascending list of distances from the start."
  (let* ((lengths (path-segment-lengths coordinates closed))
         (count (floor (length coordinates) 2))
         (last-segment (1- (length lengths)))
         (segment 0)
         (start 0d0))
    (loop for distance in distances
          collect (progn
                    (loop while (and (< segment last-segment)
                                     (>= distance (+ start (aref lengths segment))))
                          do (incf start (aref lengths segment))
                             (incf segment))
                    (let* ((next (mod (1+ segment) count))
                           (x1 (aref coordinates (* 2 segment)))
                           (y1 (aref coordinates (1+ (* 2 segment))))
                           (x2 (aref coordinates (* 2 next)))
                           (y2 (aref coordinates (1+ (* 2 next))))
                           (length (aref lengths segment))
                           (fraction (if (plusp length)
                                         (max 0d0 (min 1d0 (/ (- distance start) length)))
                                         0d0)))
                      (list (+ x1 (* fraction (- x2 x1)))
                            (+ y1 (* fraction (- y2 y1)))
                            (atan (- y2 y1) (- x2 x1))))))))

(defun path-even-distances (length
                            closed
                            count)
  "Return COUNT distances along a path of the given length, the first at its start, at equal steps: the last at its
end when the path is open, and a step short of it when it is closed, where the end is the start.

LENGTH: the length of the path.
CLOSED: true for a closed path.
COUNT: how many distances, at least one."
  (let ((step (cond ((and closed (plusp count)) (/ length count))
                    ((> count 1) (/ length (1- count)))
                    (t 0d0))))
    (loop for index below count
          collect (* index step))))

(defun path-spaced-distances (length
                              closed
                              spacing)
  "Return the distances along a path of the given length at which something is put every SPACING from its start: as
many as fit, the one at the end of an open path included when the spacing divides the length, and for a closed path
none at the end, where it would sit on the first.

LENGTH: the length of the path.
CLOSED: true for a closed path.
SPACING: the distance between one and the next, above zero."
  (let* ((steps (/ length spacing))
         (count (if closed
                    (ceiling (- steps +path-distance-tolerance+))
                    (1+ (floor (+ steps +path-distance-tolerance+))))))
    (loop for index below (max 1 count)
          collect (* index spacing))))
