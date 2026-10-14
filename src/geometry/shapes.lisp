(in-package #:borba-cad/geometry)

(defconstant +pi+ (float pi 1d0)
  "Pi as a double-float.")

(defun circle-circumference (radius)
  "Return the circumference of a circle.

RADIUS: circle radius."
  (declare (type double-float radius))
  (* 2d0 +pi+ radius))

(defun circle-area (radius)
  "Return the area of a circle.

RADIUS: circle radius."
  (declare (type double-float radius))
  (* +pi+ radius radius))

(defun circle-bounds (center-x
                      center-y
                      radius)
  "Return the bounds of a circle.

CENTER-X: circle center x.
CENTER-Y: circle center y.
RADIUS: circle radius."
  (declare (type double-float center-x center-y radius))
  (make-bounds (- center-x radius)
               (- center-y radius)
               (+ center-x radius)
               (+ center-y radius)))

(defun rectangle-area (width
                       height)
  "Return the area of a rectangle.

WIDTH: extent along x.
HEIGHT: extent along y."
  (declare (type double-float width height))
  (* width height))

(defun rectangle-perimeter (width
                            height)
  "Return the perimeter of a rectangle.

WIDTH: extent along x.
HEIGHT: extent along y."
  (declare (type double-float width height))
  (* 2d0 (+ width height)))

(declaim (inline polyline-vertex-count))
(defun polyline-vertex-count (coordinates)
  "Return the number of vertices in a flat x/y coordinate array.

COORDINATES: simple-array of double-float holding x0 y0 x1 y1 ..."
  (declare (type (simple-array double-float (*)) coordinates))
  (floor (length coordinates) 2))

(defun polyline-length (coordinates
                        closed)
  "Return the total length of a polyline, including the closing segment when CLOSED.

COORDINATES: simple-array of double-float holding x0 y0 x1 y1 ...
CLOSED: when true, add the segment from the last vertex back to the first."
  (declare (type (simple-array double-float (*)) coordinates))
  (let ((count (polyline-vertex-count coordinates))
        (total 0d0))
    (declare (type double-float total))
    (loop for index from 0 below (1- count)
          do (incf total (distance (aref coordinates (* 2 index))
                                   (aref coordinates (+ (* 2 index) 1))
                                   (aref coordinates (+ (* 2 index) 2))
                                   (aref coordinates (+ (* 2 index) 3)))))
    (when (and closed (> count 2))
      (incf total (distance (aref coordinates (- (length coordinates) 2))
                            (aref coordinates (- (length coordinates) 1))
                            (aref coordinates 0)
                            (aref coordinates 1))))
    total))

(defun polyline-bounds (coordinates)
  "Return the bounds of all vertices of a flat x/y coordinate array.

COORDINATES: simple-array of double-float with at least one vertex."
  (declare (type (simple-array double-float (*)) coordinates))
  (let ((min-x (aref coordinates 0))
        (min-y (aref coordinates 1))
        (max-x (aref coordinates 0))
        (max-y (aref coordinates 1)))
    (declare (type double-float min-x min-y max-x max-y))
    (loop for index from 2 below (length coordinates) by 2
          do (let ((x (aref coordinates index))
                   (y (aref coordinates (1+ index))))
               (setf min-x (min min-x x)
                     max-x (max max-x x)
                     min-y (min min-y y)
                     max-y (max max-y y))))
    (make-bounds min-x
                 min-y
                 max-x
                 max-y)))
