(in-package #:borba-cad/geometry)

(declaim (inline clip-segment-to-bounds))
(defun clip-segment-to-bounds (x1
                               y1
                               x2
                               y2
                               bounds)
  "Clip a segment to BOUNDS (Liang-Barsky). Returns four values, the clipped x1 y1 x2 y2,
or NIL when no part of the segment lies inside. Done in double precision so far-away
endpoints never reach single-float vertex buffers.

X1: x of the segment start.
Y1: y of the segment start.
X2: x of the segment end.
Y2: y of the segment end.
BOUNDS: the clipping rectangle."
  (declare (type double-float x1 y1 x2 y2))
  (let ((t-enter 0d0)
        (t-leave 1d0)
        (dx (- x2 x1))
        (dy (- y2 y1)))
    (declare (type double-float t-enter t-leave dx dy))
    (flet ((accept-p (direction
                      distance-to-edge)
             ;; DIRECTION is p, DISTANCE-TO-EDGE is q of the classic formulation.
             (declare (type double-float direction distance-to-edge))
             (cond ((zerop direction)
                    (>= distance-to-edge 0d0))
                   (t (let ((ratio (/ distance-to-edge direction)))
                        (declare (type double-float ratio))
                        (cond ((< direction 0d0)
                               (cond ((> ratio t-leave) nil)
                                     (t (when (> ratio t-enter) (setf t-enter ratio))
                                        t)))
                              (t
                               (cond ((< ratio t-enter) nil)
                                     (t (when (< ratio t-leave) (setf t-leave ratio))
                                        t)))))))))
      (when (and (accept-p (- dx) (- x1 (bounds-min-x bounds)))
                 (accept-p dx (- (bounds-max-x bounds) x1))
                 (accept-p (- dy) (- y1 (bounds-min-y bounds)))
                 (accept-p dy (- (bounds-max-y bounds) y1)))
        (values (+ x1 (* t-enter dx))
                (+ y1 (* t-enter dy))
                (+ x1 (* t-leave dx))
                (+ y1 (* t-leave dy)))))))
