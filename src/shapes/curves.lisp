(in-package #:borba-cad/shapes)

;;;; Flattening curves: a cubic or quadratic Bezier curve as the points of a polyline that never strays from it by
;;;; more than a tolerance. Every shape of the library is in the end a list of polylines, because a drawing holds
;;;; segments, and a drawing can be enlarged until it is as big as a gate: the tolerance is in the units of the shape.

(defconstant +curve-tolerance+ 0.02d0
  "The most that the points of a flattened curve may stray from the curve, in the units of the shape (a shape is 100
units high, which is 100 millimetres as it comes).")

(defconstant +maximum-curve-segments+ 256
  "Most segments that one curve is cut into.")

(defun segments-for (control-length
                     tolerance)
  "Return into how many segments to cut a curve so that it stays within a tolerance of the polyline: the number
grows with the square root of the length of its control polygon, which bounds how far it bends.

CONTROL-LENGTH: the length of the control polygon of the curve.
TOLERANCE: the most that the polyline may stray from the curve."
  (max 1
       (min +maximum-curve-segments+
            (ceiling (sqrt (/ (* 0.75d0 control-length) tolerance))))))

(defun point-distance (a
                       b)
  "Return the distance between two points.

A: a cons of x and y.
B: a cons of x and y."
  (let ((dx (- (car b) (car a)))
        (dy (- (cdr b) (cdr a))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun flatten-cubic (p0
                      p1
                      p2
                      p3
                      &optional (tolerance +curve-tolerance+))
  "Return the points of a polyline that follows a cubic Bezier curve, after its start and up to and including its end,
as a list of conses of x and y.

P0: the start, a cons of x and y.
P1: the first control point.
P2: the second control point.
P3: the end.
TOLERANCE: the most that the polyline may stray from the curve."
  (let ((count (segments-for (+ (point-distance p0 p1) (point-distance p1 p2) (point-distance p2 p3))
                             tolerance)))
    (loop for step from 1 to count
          collect (let* ((u (/ step (float count 1d0)))
                         (v (- 1d0 u))
                         (a (* v v v))
                         (b (* 3d0 v v u))
                         (c (* 3d0 v u u))
                         (d (* u u u)))
                    (cons (+ (* a (car p0)) (* b (car p1)) (* c (car p2)) (* d (car p3)))
                          (+ (* a (cdr p0)) (* b (cdr p1)) (* c (cdr p2)) (* d (cdr p3))))))))

(defun flatten-quadratic (p0
                          p1
                          p2
                          &optional (tolerance +curve-tolerance+))
  "Return the points of a polyline that follows a quadratic Bezier curve, after its start and up to and including its
end, as a list of conses of x and y.

P0: the start, a cons of x and y.
P1: the control point.
P2: the end.
TOLERANCE: the most that the polyline may stray from the curve."
  (flet ((third-of (from to)
           (cons (+ (car from) (* (/ 2d0 3d0) (- (car to) (car from))))
                 (+ (cdr from) (* (/ 2d0 3d0) (- (cdr to) (cdr from)))))))
    (flatten-cubic p0
                   (third-of p0 p1)
                   (third-of p2 p1)
                   p2
                   tolerance)))
